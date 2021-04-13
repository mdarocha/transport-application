module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Browser.Events exposing (onResize)
import Element exposing (..)
import Element.Input exposing (button)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Flags
import Json.Encode as Encoder
import Json.Decode as Decoder
import Process
import Task
import Pages.Authentication as AuthenticationPage
import Pages.Account as AccountPage
import Pages.NotFound as NotFoundPage
import Pages.Tracker as TrackerPage
import Pages.PlaceOrder as PlaceOrderPage
import Pages.Jobs as JobsPage
import Routes exposing (Route)
import Session exposing (Session, AuthStatus(..), PendingEventStatus(..))
import Styles exposing (colorBrand, colorGrey)
import Url
import Ports.WebSocket as WebSocket
import Ports.Geolocation as Geolocation
import Translations exposing (translate)
import Http
import Jwt.Http
import Utils

main : Program Encoder.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Model
    = UnknownPage Session
    | TrackerModel TrackerPage.Model
    | AuthenticationModel AuthenticationPage.Model
    | PlaceOrderModel PlaceOrderPage.Model
    | AccountModel AccountPage.Model
    | JobsModel JobsPage.Model


-- MSG

type WebSocketMsg
    = Opened
    | Closed
    | WentOnline
    | WentOffline
    | NewMsg (Maybe WebSocket.WebSocketMessage)

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | WebSocketChanged WebSocketMsg
    | BrowserSizeChanged (Int, Int)
    | HeaderMenuToggle
    | GotEventStatus (Result Http.Error PendingEventStatus)
    | TrackerMsg TrackerPage.Msg
    | AuthenticationMsg AuthenticationPage.Msg
    | PlaceOrderMsg PlaceOrderPage.Msg
    | AccountMsg AccountPage.Msg
    | JobsMsg JobsPage.Msg

-- INIT


init : Encoder.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init unparsedFlags url key =
    let
        flags = Flags.fromJson unparsedFlags
        session = Session.fromFlags flags key
        (model, changeRouteCmd) = changeRoute (Routes.fromUrl url) (UnknownPage session)
    in
    ( model
    , Cmd.batch
        [ changeRouteCmd
        , performCheckEventStatus session
        ]
    )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            toSession model
    in
    case ( msg, model ) of
        ( LinkClicked request, _ ) ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl session.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            let
                ( routeModel, routeMsg ) = changeRoute (Routes.fromUrl url) model
            in
            ( routeModel
            , Cmd.batch
                [ routeMsg
                , Geolocation.stop ()
                ]
            )

        ( WebSocketChanged wsMsg, _ ) ->
            let
                ( newSession, cmds ) = updateWebSocket wsMsg session
                newModel = setSession model newSession
            in
            ( newModel, cmds )

        ( BrowserSizeChanged size, _ ) ->
            let
                newSession = { session | window = size }
                newModel = setSession model newSession
            in
            ( newModel, Cmd.none )

        ( HeaderMenuToggle, _ ) ->
            let
                newSession = { session | headerMenuExpanded = not session.headerMenuExpanded }
                newModel = setSession model newSession
            in
            ( newModel, Cmd.none )

        ( GotEventStatus result, _ ) ->
            case result of
                Ok status ->
                    let
                        newSession = { session | pendingEvent = status }
                        newModel = setSession model newSession
                        navCmd =
                            case ( model, status ) of
                                ( PlaceOrderModel _, OrderInProgress ) ->
                                    Nav.pushUrl session.key
                                        <| Routes.toString <| Routes.Tracker
                                ( _, _ ) ->
                                    Cmd.none
                    in
                    ( newModel, navCmd )
                _ ->
                    ( model, Cmd.none )
        ( TrackerMsg trackerMsg, TrackerModel trackerModel ) ->
            TrackerPage.update trackerMsg trackerModel |> mapTrackerPage

        ( AuthenticationMsg authMsg, AuthenticationModel authModel ) ->
            AuthenticationPage.update authMsg authModel |> mapAuthenticationPage

        ( PlaceOrderMsg orderMsg, PlaceOrderModel orderModel ) ->
            PlaceOrderPage.update orderMsg orderModel |> mapPlaceOrderPage

        ( AccountMsg accountMsg, AccountModel accountModel ) ->
            AccountPage.update accountMsg accountModel |> mapAccountPage

        ( JobsMsg jobsMsg, JobsModel jobsModel ) ->
            JobsPage.update jobsMsg jobsModel |> mapJobsPage

        ( _, _ ) ->
            ( model, Cmd.none )


updateWebSocket : WebSocketMsg -> Session -> ( Session, Cmd Msg )
updateWebSocket msg session =
    case msg of
        WentOnline ->
            let
                cmd =
                    if not session.isOnline then
                        WebSocket.open ()
                    else
                        Cmd.none
            in
            ( session, cmd )

        WentOffline ->
            ( { session | isOnline = False }, Cmd.none )

        Closed ->
            let
                timesTried =
                    case session.wsStatus of
                        Session.Closed times -> times
                        _ -> 0

                (newSession, cmd) =
                    if timesTried < 10 then
                        ( { session | wsStatus = Session.Closed (timesTried + 1) }
                        , WebSocket.openAfterDelay 500 )
                    else
                        ({ session | wsStatus = Session.Closed timesTried, isOnline = False }
                        , WebSocket.openAfterDelay 5000 )
            in
            ( newSession, cmd )

        Opened ->
            let
                cmd =
                    case session.auth of
                        LoggedIn token _ -> WebSocket.Authenticate token |> WebSocket.send
                        _ -> Cmd.none
            in
            ( { session | wsStatus = Session.Open, isOnline = True }, cmd )

        NewMsg m ->
            case m of
                Just (WebSocket.AuthenticationResult isSuccess) ->
                    if isSuccess then
                        ( { session | wsStatus = Session.Authenticated }, Cmd.none )
                    else
                        ( session, Cmd.none )
                _ ->
                    ( session, Cmd.none )

-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.wentOnline (\_ -> WebSocketChanged WentOnline)
        , WebSocket.wentOffline (\_ -> WebSocketChanged WentOffline)
        , WebSocket.opened (\_ -> WebSocketChanged Opened)
        , WebSocket.closed (\_ -> WebSocketChanged Closed)
        , WebSocket.received (\msg -> WebSocketChanged <| NewMsg msg)
        , onResize (\w h -> BrowserSizeChanged (w, h))
        , case model of
            JobsModel _ -> JobsPage.subscription |> Sub.map JobsMsg
            TrackerModel _ -> TrackerPage.subscription |> Sub.map TrackerMsg
            _ -> Sub.none
        ]


-- HTTP


eventStatusDecoder : Decoder.Decoder PendingEventStatus
eventStatusDecoder =
    Decoder.map2 Tuple.pair
        (Decoder.field "hasOrder" Decoder.bool)
        (Decoder.field "hasJob" Decoder.bool)
    |> Decoder.andThen (\(hasOrder, hasJob) ->
        if hasJob then
            Decoder.succeed JobInProgress
        else if hasOrder then
            Decoder.succeed OrderInProgress
        else
            Decoder.succeed NoEvent)

performCheckEventStatus : Session -> Cmd Msg
performCheckEventStatus session =
    case session.auth of
        Anonymous -> Cmd.none
        LoggedIn token _ ->
            let
                url = Utils.buildUrl session.api [ "tracker", "status" ] []
            in
            Jwt.Http.get token
                { url = url
                , expect = Http.expectJson GotEventStatus eventStatusDecoder
                }



-- PAGES


changeRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRoute route model =
    let
        session =
            toSession model
    in
    case route of
        Nothing ->
            ( UnknownPage session, Cmd.none )

        Just Routes.Tracker ->
            TrackerPage.init session |> mapTrackerPage

        Just (Routes.Authentication redirect) ->
            AuthenticationPage.init session redirect |> mapAuthenticationPage

        Just Routes.Order ->
            PlaceOrderPage.init session |> mapPlaceOrderPage

        Just Routes.Account ->
            AccountPage.init session |> mapAccountPage

        Just Routes.Root ->
            let
                url =
                    case session.auth of
                        Anonymous -> "/order"
                        LoggedIn _ user ->
                            if user.isDriver then
                                Routes.toString Routes.Jobs
                            else
                                Routes.toString Routes.Order
            in
            ( model, Nav.replaceUrl session.key url )

        Just Routes.Jobs ->
            JobsPage.init session |> mapJobsPage

        _ ->
            ( UnknownPage session, Cmd.none )


-- SESSION

toSession : Model -> Session
toSession model =
    case model of
        UnknownPage session -> session
        TrackerModel tracker -> tracker.session
        AuthenticationModel auth -> auth.session
        PlaceOrderModel order -> order.session
        AccountModel account -> account.session
        JobsModel jobs -> jobs.session

setSession : Model -> Session -> Model
setSession model session =
    case model of
        UnknownPage _ -> UnknownPage session
        TrackerModel tracker -> TrackerModel { tracker | session = session }
        AuthenticationModel auth -> AuthenticationModel { auth | session = session }
        PlaceOrderModel order -> PlaceOrderModel { order | session = session }
        AccountModel account -> AccountModel { account | session = session }
        JobsModel jobs -> JobsModel { jobs | session = session }


-- MAPPERS

mapTrackerPage : ( TrackerPage.Model, Cmd TrackerPage.Msg ) -> ( Model, Cmd Msg )
mapTrackerPage ( model, msg ) =
    ( TrackerModel model, Cmd.map TrackerMsg msg )

mapAuthenticationPage : ( AuthenticationPage.Model, Cmd AuthenticationPage.Msg ) -> ( Model, Cmd Msg )
mapAuthenticationPage ( model, msg ) =
    ( AuthenticationModel model, Cmd.map AuthenticationMsg msg )

mapPlaceOrderPage : ( PlaceOrderPage.Model, Cmd PlaceOrderPage.Msg ) -> ( Model, Cmd Msg )
mapPlaceOrderPage ( model, msg ) =
    ( PlaceOrderModel model, Cmd.map PlaceOrderMsg msg )

mapAccountPage : ( AccountPage.Model, Cmd AccountPage.Msg ) -> ( Model, Cmd Msg )
mapAccountPage ( model, msg ) =
    ( AccountModel model, Cmd.map AccountMsg msg )

mapJobsPage : ( JobsPage.Model, Cmd JobsPage.Msg ) -> ( Model, Cmd Msg )
mapJobsPage ( model, msg ) =
    ( JobsModel model, Cmd.map JobsMsg msg )


-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        session = toSession model

        ( viewportHeight, viewportWidth ) = session.window
        device = classifyDevice { height = viewportHeight, width = viewportWidth }

        ( title, content ) =
            viewContent device model

        page =
            layout [ height fill ] <|
                column [ width fill, height fill ]
                    [ viewOnlineStatus session
                    , viewHeader device session model
                    , el [ Region.mainContent, width fill, height fill ] content
                    ]
    in
    { title = title ++ " | " ++ (translate session.language "TransportApplication")
    , body = [ page ]
    }


viewContent : Device -> Model -> ( String, Element Msg )
viewContent device model =
    case model of
        TrackerModel tracker ->
            TrackerPage.view device tracker |> mapElement TrackerMsg

        AuthenticationModel auth ->
            AuthenticationPage.view device auth |> mapElement AuthenticationMsg

        PlaceOrderModel order ->
            PlaceOrderPage.view device order |> mapElement PlaceOrderMsg

        AccountModel account ->
            AccountPage.view device account |> mapElement AccountMsg

        JobsModel jobs ->
            JobsPage.view device jobs |> mapElement JobsMsg

        UnknownPage session ->
            NotFoundPage.view device session

viewOnlineStatus : Session -> Element Msg
viewOnlineStatus session =
    if not session.isOnline then
        column
        [ width fill
        , alignTop
        , Background.color Styles.colorError
        , padding 4
        , centerX
        ]
        [ el [ centerX ] (text (translate session.language "You are offline."))
        , el [ centerX ] (text (translate session.language "Some features will not work"))
        ]
    else
        row [] []

mapElement : (pageMsg -> msg) -> ( String, Element pageMsg ) -> ( String, Element msg )
mapElement toMsg ( title, pageContent ) =
    let
        content =
            Element.map toMsg pageContent
    in
    ( title, content )


viewHeader : Device -> Session -> Model -> Element Msg
viewHeader device session model =
    let
        authLink =
            case session.auth of
                LoggedIn _ user ->
                    viewHeaderLink (Routes.toString Routes.Account) (user.name ++ " " ++ user.surname) (isLinkActive model Routes.Account)
                Anonymous ->
                    let
                        route = Routes.Authentication Nothing
                    in
                    viewHeaderLink (Routes.toString route) (translate session.language "Login") (isLinkActive model route)

        orderLink =
            case session.auth of
                LoggedIn _ user ->
                    if user.isClient then
                        viewHeaderLink (Routes.toString Routes.Order) (translate session.language "Place an order") (isLinkActive model Routes.Order)
                    else
                        \b -> none
                _ -> viewHeaderLink (Routes.toString Routes.Order) (translate session.language "Place an order") (isLinkActive model Routes.Order)


        jobsLink =
            case session.auth of
                LoggedIn _ user ->
                    if user.isDriver then
                        viewHeaderLink (Routes.toString Routes.Jobs) (translate session.language "Delivery jobs") (isLinkActive model Routes.Jobs)
                    else
                        \b -> none
                _ -> viewHeaderLink (Routes.toString Routes.Jobs) (translate session.language "Delivery jobs") (isLinkActive model Routes.Jobs)

    in
    row
        [ width fill
        , alignTop
        , Background.color colorGrey
        , Region.navigation
        ]
        [ link
            [ paddingEach { top = 15, right = 30, left = 15, bottom = 15 }
            , Background.color colorBrand
            , Border.roundEach { topLeft = 0, bottomLeft = 0, topRight = 5, bottomRight = 30 }
            , Font.color (rgba 0.1 0.1 0.1 0.9)
            , Font.family [ Font.typeface "Pacifico" ]
            ]
            { url = "/", label = text (translate session.language "TransportApplication") }
        , el [ paddingXY 15 0, alignLeft ]
            <| viewHeaderNotification device session
        , viewHeaderMenu device session
            [ orderLink
            , jobsLink
            , authLink
            ]
        ]

viewHeaderMenu : Device -> Session -> List (Bool -> Element Msg) -> Element Msg
viewHeaderMenu device session items =
    case device.class of
        Phone ->
            let
                menu =
                    if session.headerMenuExpanded then
                        column
                            [ width fill
                            , Background.color colorGrey
                            , Border.color (rgba 0 0 0 1)
                            ]
                            (List.map (\i -> i True) items)
                    else
                        column [] []
            in
            row [ padding 8, spacing 15, width fill , below menu ]
                [ button [ alignRight, padding 5 ] { onPress = Just HeaderMenuToggle, label = text (translate session.language "Menu") } ]
        _ ->
            row [ padding 8 , spacing 15 , width fill ] (List.map (\i -> i False) items)

viewHeaderNotification : Device -> Session -> Element Msg
viewHeaderNotification device session =
    let
        container value =
            el [ Font.italic ]
                <| row [] value
    in
    case session.pendingEvent of
        NoEvent ->
            none
        OrderInProgress ->
            container <| [ Styles.faIcon " ", text <| translate session.language "Order is in progress" ]
        JobInProgress ->
            container <| [ Styles.faIcon " ", text <| translate session.language "Delivery is in progress" ]


isLinkActive : Model -> Route -> Bool
isLinkActive model route =
    case (model, route) of
        (AuthenticationModel _, Routes.Authentication _) -> True
        (TrackerModel _, Routes.Tracker) -> True
        (PlaceOrderModel _, Routes.Order) -> True
        (AccountModel _, Routes.Account) -> True
        (JobsModel _, Routes.Jobs) -> True
        (_, _) -> False

viewHeaderLink : String -> String -> Bool -> Bool -> Element Msg
viewHeaderLink route label isActive isPhone =
    if not isPhone then
        let
            backgroundColor =
                if isActive then colorBrand else rgba 0 0 0 0
        in
        link
            [ alignRight
            , pointer
            , padding 5
            , mouseOver [ Border.color (rgba 0 0 0 1) ]
            , Background.color backgroundColor
            , Border.color (rgba 0 0 0 0)
            , Border.width 2
            , Border.rounded 6
            ]
            { url = route, label = text label }
    else
        let
            backgroundColor =
                if isActive then colorBrand else rgba 0 0 0 0
        in
        link
            [ centerX
            , width fill
            , pointer
            , padding 15
            , Background.color backgroundColor
            , Border.color (rgba 0 0 0 1)
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            ]
            { url = route, label = text label }
