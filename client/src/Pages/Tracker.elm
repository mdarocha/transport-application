module Pages.Tracker exposing (Model, Msg, init, update, view, subscription)

import Element exposing (..)
import Element.Font as Font
import Types exposing (Place, placeDecoder, OrderStatus(..), orderStatusDecoder, Latitude(..), Longitude(..))
import Session exposing (Session, AuthStatus(..), PendingEventStatus(..))
import Ports.GeoMap as Map exposing (MapPosition, geomap, geomarker, defaultMapPosition)
import Ports.WebSocket as WebSocket exposing (WebSocketMessage)
import Browser.Navigation as Nav
import Json.Decode as Decode
import Http
import Jwt.Http
import Routes
import Utils exposing (buildUrl)
import Styles
import Translations exposing (translate)

-- MODEL

type alias Order =
    { id : Int
    , from : Place
    , to : Place
    , status : OrderStatus
    }

type TrackedOrderStatus
    = ErrorGetting
    | Loading
    | Loaded Order

type alias Model =
    { session : Session
    , map : MapPosition
    , token : String
    , order : TrackedOrderStatus
    , driverPosition : Maybe (Latitude, Longitude)
    }


-- MSG

type Msg
    = GotTrackedOrder (Result Http.Error Order)
    | GotMessage (Maybe WebSocketMessage)

-- INIT

init : Session -> ( Model, Cmd Msg )
init session =
    let
        model token =
            Model session defaultMapPosition token Loading Nothing
    in
    case session.auth of
        LoggedIn token user ->
            if user.isClient then
                ( model token, getTrackedOrder session.api token )
            else
                ( model "", Nav.pushUrl session.key "/" )
        Anonymous ->
            let
                thisUrl = Routes.toString Routes.Jobs |> Just
            in
            ( model "", Nav.pushUrl session.key <| Routes.toString <| Routes.Authentication thisUrl )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTrackedOrder result ->
            case result of
                Ok order ->
                    ( { model | order = Loaded order }, Cmd.none )
                Err _ ->
                    ( { model | order = ErrorGetting }, Nav.pushUrl model.session.key "/" )

        GotMessage (Just (WebSocket.LocationUpdate lat lng _)) ->
            let
                oldMap = model.map
                newMap = { oldMap | latitude = lat, longitude = lng }
            in
            ( { model | driverPosition = Just ( lat, lng ), map = newMap }, Cmd.none )

        GotMessage (Just (WebSocket.OrderStatusUpdate status)) ->
            case model.order of
                Loaded order ->
                    case status of
                        Completed ->
                            let
                                oldSession = model.session
                                newSession = { oldSession | pendingEvent = NoEvent }
                            in
                            ( { model | session = newSession }
                            , Nav.pushUrl model.session.key <| Routes.toString Routes.Account
                            )
                        _ ->
                            let
                                newOrder = { order | status = status }
                            in
                            ( { model | order = Loaded newOrder }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        GotMessage _ ->
            ( model, Cmd.none )

-- HTTP

orderDecoder : Decode.Decoder Order
orderDecoder =
    Decode.map4 Order
        ( Decode.field "id" Decode.int )
        ( Decode.field "from" placeDecoder )
        ( Decode.field "to" placeDecoder )
        ( Decode.field "status" orderStatusDecoder )


getTrackedOrder : String -> String -> Cmd Msg
getTrackedOrder api token =
    let
        url = buildUrl api [ "tracker", "current" ] []
    in
    Jwt.Http.get token
        { url = url
        , expect = Http.expectJson GotTrackedOrder orderDecoder
        }


-- SUBSCRIPTIONS

subscription : Sub Msg
subscription =
    Sub.batch
        [ WebSocket.received GotMessage
        ]


-- VIEW

view : Device -> Model -> ( String, Element Msg )
view device model =
    let
        content =
            Styles.responsiveMapContentView device model viewOrderStatus viewMap
    in
    ( translate model.session.language "Tracker", content )

viewMap : Model -> Element Msg
viewMap model =
    if model.session.isWebGL then
        let
            markers =
                case model.driverPosition of
                    Just (lat, lng) -> [ geomarker lng lat ]
                    Nothing -> []
        in
        geomap [ width fill, height fill ]
            [ Map.position model.map ] markers
    else
        el [ width fill, height fill ] <|
            el [ centerX, centerY ]
            <| paragraph [ padding 10 ] [ text <| translate model.session.language "Please enable WebGL support in your browser. See https://get.webgl.org/" ]

viewOrderStatus : Model -> Element Msg
viewOrderStatus model =
    case model.order of
        Loaded order ->
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
                [ place order.from
                , el [ centerX ] <| Styles.faIcon ""
                , place order.to
                , el [ width fill, height fill ]
                    <| paragraph [ centerX, centerY, Font.bold, Font.size 26 ]
                        [ text <| orderStatusToString model.session.language order.status ]
                ]

        Loading ->
            column [ width fill, height fill ]
                [ el [ centerX ] <| text <| translate model.session.language "Loading..." ]

        ErrorGetting ->
            column [ width fill, height fill ]
                [ el [ centerX ] <| text <| translate model.session.language "An error has occured getting order information" ]

orderStatusToString : String -> OrderStatus -> String
orderStatusToString lang status =
    case status of
        SearchingForDriver ->
            translate lang "Searching for drivers to pick up your order..."
        Delivering ->
            translate lang "Your order is being delivered"
        Completed ->
            translate lang "Your delivery is complete"
        Failed ->
            translate lang "Your delivery has failed"
