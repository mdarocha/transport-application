module Pages.Jobs exposing (Model, Msg, init, update, view, subscription)

import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Input as Input
import Element.Border as Border
import Element.Events as Events
import Browser.Navigation as Nav
import Ports.WebSocket as WebSocket exposing (WebSocketMessage)
import Ports.GeoMap as Map exposing (MapPosition, geomap, geomarker, defaultMapPosition)
import Ports.Geolocation as Geolocation exposing (GeoPosition)
import Routes
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Session exposing (Session, AuthStatus(..), PendingEventStatus(..))
import Styles
import Translations exposing (translate)
import Time exposing (Posix)
import Types exposing (Place, placeDecoder, OrderStatus(..), orderStatusDecoder)
import Utils exposing (buildUrl)
import Jwt.Http
import Http

-- MODEL

type Geolocation
    = NotWatching
    | Watching GeoPosition
    | GeolocationError Geolocation.Error

type alias Job =
    { id : Int
    , from : Place
    , to : Place
    , routeDistance : Float
    , toBeginDistance : Float
    , startedTime : Posix
    , status : OrderStatus
    }

type Jobs
    = ErrorGettingJobs
    | LoadingJobs
    | Jobs (List Job)

type alias Model =
    { session : Session
    , token : String
    , map : MapPosition
    , geolocation : Geolocation
    , locationUpdatesSent : Int
    , currentJob : Maybe Job
    , jobs : Jobs
    }


-- MSG


type Msg
    = GotJobs (Result Http.Error (List Job))
    | GotCurrentJob (Result Http.Error (Maybe Job))
    | GotMessage (Maybe WebSocketMessage)
    | StartGeolocationPressed
    | GeolocationUpdate (Result Geolocation.Error GeoPosition)
    | LocationUpdateTimer Posix
    | JobClicked Job
    | CompleteJobClicked
    | CancelJobClicked
    | GotJobAccept Job (Result Http.Error ())
    | GotJobCancel (Result Http.Error ())
    | GotJobComplete (Result Http.Error ())


-- INIT

init : Session -> ( Model, Cmd Msg )
init session =
    let
        model token =
            Model session token defaultMapPosition NotWatching 0 Nothing LoadingJobs
    in
    case session.auth of
        LoggedIn token user ->
            if user.isDriver then
                ( model token, Cmd.none )
            else
                ( model "", Nav.pushUrl session.key "/" )
        Anonymous ->
            let
                thisUrl = Routes.toString Routes.Jobs |> Just
            in
            ( model "", Nav.pushUrl session.key (Routes.toString <| Routes.Authentication thisUrl) )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJobs result ->
            case result of
                Ok jobs ->
                    ( { model | jobs = Jobs jobs }, Cmd.none )
                Err _ ->
                    ( { model | jobs = ErrorGettingJobs }, Cmd.none )

        GotCurrentJob result ->
            case result of
                Ok job ->
                    ( { model | currentJob = job }, Cmd.none )
                Err _ ->
                    ( model, Cmd.none )

        GotMessage (Just WebSocket.RefreshJobs) ->
            ( model, getAvailableJobs model.session.api model.token )

        GotMessage _ ->
            ( model, Cmd.none )

        StartGeolocationPressed ->
            ( model
            , Cmd.batch
                [ Geolocation.start ()
                , getCurrentJob model.session.api model.token
                ]
            )

        GeolocationUpdate result ->
            case result of
                Ok newLocation ->
                    ( { model | geolocation = Watching newLocation }, Cmd.none )
                Err error ->
                    ( { model | geolocation = GeolocationError error }, Cmd.none )

        LocationUpdateTimer time ->
            case model.geolocation of
                Watching pos ->
                    let
                        message = WebSocket.LocationUpdate pos.lat pos.lng time
                        cmds =
                            if modBy 2 model.locationUpdatesSent == 1 then
                                getAvailableJobs model.session.api model.token
                            else
                                Cmd.none
                    in
                    ( { model | locationUpdatesSent = model.locationUpdatesSent + 1 }
                    , Cmd.batch [ WebSocket.send message, cmds ] )
                _ ->
                    ( model, Cmd.none )

        JobClicked job ->
            ( model, acceptJob model.session.api model.token job )

        CompleteJobClicked ->
            case model.currentJob of
                Just job ->
                    ( model, completeJob model.session.api model.token job )
                Nothing ->
                    ( model, Cmd.none )

        CancelJobClicked ->
            case model.currentJob of
                Just job ->
                    ( model, cancelJob model.session.api model.token job )
                Nothing ->
                    ( model, Cmd.none )

        GotJobAccept job result ->
            case result of
                Ok _ ->
                    let
                        oldSession = model.session
                        newSession = { oldSession | pendingEvent = JobInProgress }
                    in
                    ( { model | session = newSession, currentJob = Just job }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        GotJobCancel result ->
            case result of
                Ok _ ->
                    let
                        oldSession = model.session
                        newSession = { oldSession | pendingEvent = NoEvent }
                    in
                    ( { model | session = newSession, currentJob = Nothing }, Cmd.none )
                _ ->
                    ( model, Cmd.none )


        GotJobComplete result ->
            case result of
                Ok _ ->
                    let
                        oldSession = model.session
                        newSession = { oldSession | pendingEvent = NoEvent }
                    in
                    ( { model | session = newSession, currentJob = Nothing }, Cmd.none )
                _ ->
                    ( model, Cmd.none )




-- HTTP

jobDecoder : Decode.Decoder Job
jobDecoder =
    Decode.map7 Job
        (Decode.field "id" Decode.int)
        (Decode.field "from" placeDecoder)
        (Decode.field "to" placeDecoder)
        (Decode.field "routeDistanceMeters" Decode.float)
        (Decode.field "beginDistanceMeters" Decode.float)
        (Decode.field "startTime" Decode.int
            |> Decode.andThen (\v -> Time.millisToPosix v |> Decode.succeed))
        (Decode.field "status" orderStatusDecoder)

jobsDecoder : Decode.Decoder (List Job)
jobsDecoder =
    jobDecoder |> Decode.list

currentJobDecoder : Decode.Decoder (Maybe Job)
currentJobDecoder =
    Decode.oneOf
        [ jobDecoder |> Decode.map Just
        , Decode.bool
            |> Decode.andThen (\v ->
                if not v then
                    Decode.succeed Nothing
                else
                    Decode.fail "unknown value passed")
        ]

encodeJobId : Job -> Encode.Value
encodeJobId job =
    Encode.object
        [ ( "jobId", Encode.int job.id ) ]

getAvailableJobs : String -> String -> Cmd Msg
getAvailableJobs api token =
    let
        url = buildUrl api [ "job", "available" ] []
    in
    Jwt.Http.get token
        { url = url
        , expect = Http.expectJson GotJobs jobsDecoder
        }

getCurrentJob : String -> String -> Cmd Msg
getCurrentJob api token =
    let
        url = buildUrl api [ "job", "current" ] []
    in
    Jwt.Http.get token
        { url = url
        , expect = Http.expectJson GotCurrentJob currentJobDecoder
        }

updateJobStatus : String -> (Result Http.Error () -> Msg) -> String -> String -> Job -> Cmd Msg
updateJobStatus part msg api token job =
    let
        url = buildUrl api [ "job", part ] []
    in
    Jwt.Http.post token
        { url = url
        , expect = Http.expectWhatever msg
        , body = Http.jsonBody (encodeJobId job)
        }

acceptJob : String -> String -> Job -> Cmd Msg
acceptJob api token job =
    updateJobStatus "accept" (GotJobAccept job) api token job

cancelJob : String -> String -> Job -> Cmd Msg
cancelJob =
    updateJobStatus "cancel" GotJobCancel

completeJob : String -> String -> Job -> Cmd Msg
completeJob =
    updateJobStatus "complete" GotJobComplete


-- SUBSCRIPTIONS


subscription : Sub Msg
subscription =
    Sub.batch
        [ WebSocket.received GotMessage
        , Geolocation.update GeolocationUpdate
        , Time.every 1500 LocationUpdateTimer
        ]



-- VIEW


view : Device -> Model -> ( String, Element Msg )
view device model =
    let
        content =
            Styles.responsiveMapContentView device model viewSelector viewMap
    in
    ( translate model.session.language "Delivery jobs", content )

viewMap : Model -> Element Msg
viewMap model =
    if model.session.isWebGL then
        let
            markers =
                case model.geolocation of
                    Watching pos -> [ geomarker pos.lng pos.lat ]
                    _ -> []
        in
        geomap [ width fill, height fill ]
            [ Map.position model.map ] markers
    else
        el [ width fill, height fill ] <|
            el [ centerX, centerY ]
            <| paragraph [ padding 10 ] [ text <| translate model.session.language "Please enable WebGL support in your browser. See https://get.webgl.org/" ]

viewSelector : Model -> Element Msg
viewSelector model =
    let
        currentOrList =
            case model.currentJob of
                Nothing ->
                    viewJobsList model.session model.jobs
                Just job ->
                    viewCurrentJobStatus model.session job
    in
    case model.geolocation of
        NotWatching ->
            viewStartScreen model.session
        Watching _ ->
            currentOrList
        GeolocationError err ->
            case err of
                Geolocation.PermissionDenied ->
                    viewPermissionError model.session
                _ ->
                    currentOrList

viewStartScreen : Session -> Element Msg
viewStartScreen session =
    column [ centerY, width fill, padding 15, spacing 10 ]
    [ el [ Font.size 30, centerX, paddingXY 5 15 ] <|
        text <| translate session.language "Start delivering"
    , el [ centerX ] <|
        paragraph [ spacing 10 ]
        [ text <| translate session.language "To work deliveries, make sure your Location Services are enabled,"
        , text " "
        , text <| translate session.language "and you have given TransportApplication permission to use location"
        ]
    , el [ paddingXY 10 20, width fill ]
        <| Styles.largeButton StartGeolocationPressed
        <| translate session.language "Begin"
    ]


viewPermissionError : Session -> Element Msg
viewPermissionError session =
    column [ centerY, width fill, padding 15, spacing 10 ]
    [ el [ centerX ] <|
        paragraph [ spacing 10 ]
        [ text <| translate session.language "You have to give TransportApplication permission to use location" ]
    ]

viewJobsList : Session -> Jobs -> Element Msg
viewJobsList session jobsState =
    let
        info t =
            el [ centerX, Font.size 22, padding 25 ] <| text t
    in
    case jobsState of
        ErrorGettingJobs ->
            info <| translate session.language "Error getting available jobs"
        LoadingJobs ->
            info <| translate session.language "Loading..."
        Jobs jobs ->
            if List.length jobs == 0 then
                info <| translate session.language "No jobs currently available"
            else
                List.map (viewJob session) jobs
                |> column
                    [ width fill
                    , scrollbars
                    , paddingXY 10 25
                    , spacing 20
                    ]

viewJob : Session -> Job -> Element Msg
viewJob session job =
    column
        [ width fill, spacing 15, padding 20
        , Border.width 1, Border.rounded 5
        , Border.color Styles.colorAccent
        , Border.shadow
            { offset = (0, 0)
            , size = 5
            , blur = 5
            , color = rgba255 206 206 206 0.1
            }
        , pointer
        , mouseOver
            [ Border.color Styles.colorBrand
            , Border.shadow
                { offset = (0, 5)
                , size = 5
                , blur = 5
                , color = rgba255 206 206 206 0.5
                }
            ]
        , Events.onClick (JobClicked job)
        ]
        [ wrappedRow [ spacing 15, width fill ]
            [ column [ width fill, alignLeft, spacing 5 ]
                [ el [ Font.semiBold, alignLeft, Font.size 21 ] (text job.from.name)
                , el [ Font.italic, alignLeft ] (text job.from.address)
                ]
            , el [ centerX, Font.italic ] <| Styles.faIcon ""
            , column [ width fill, alignRight, spacing 5 ]
                [ el [ Font.semiBold, alignRight, Font.size 21 ] (text job.to.name)
                , el [ Font.italic, alignRight ] (text job.to.address)
                ]
            ]
        , row [ spacing 25 ]
            [ el [ ] (text <|
                translate session.language "To Start: "
                ++
                viewDistance job.toBeginDistance)
            , el [ ] (text <|
                translate session.language "Length: "
                ++
                viewDistance job.routeDistance)
            ]
        ]

viewCurrentJobStatus : Session -> Job -> Element Msg
viewCurrentJobStatus session job =
    let
        place p =
            column [ spacing 10, width fill ]
                [ el [ Font.size 24, centerX ] <| text p.name
                , el [ Font.italic, centerX ] <| text p.address
                ]
    in
    column
        [ width fill, height fill
        , spacing 20, padding 15
        ]
        [ place job.from
        , el [ centerX ] <| Styles.faIcon ""
        , place job.to
        , el [ width fill, height fill ]
            <| el [ centerX, centerY, Font.bold, Font.size 26 ]
            <| text <| String.toUpper
            <| translate session.language "In progress"
        , el [ width fill, alignBottom, padding 20 ]
            <| Styles.largeButton CompleteJobClicked
            <| translate session.language "Complete"
        ]


viewDistance : Float -> String
viewDistance distance =
    if distance < 1000.0 then
        (distance |> round |> String.fromInt)
        ++ " m"
    else
        (distance / 1000.0 |> round |> String.fromInt)
        ++ " km"
