module Pages.PlaceOrder exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Font as Font
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Element.Background as Background
import Element.Region as Region
import Browser.Navigation as Nav
import Session exposing (Session, AuthStatus(..), PendingEventStatus(..))
import Routes exposing (Route)
import Ports.GeoMap as Map exposing (MapPosition, geomap, geomarker, defaultMapPosition)
import Styles
import Json.Decode as Decoder
import Json.Encode as Encoder
import Utils exposing (buildUrl)
import Url.Builder as UrlBuilder
import Translations exposing (translate)
import Jwt.Http
import Http
import Types exposing(Latitude(..), Longitude(..), Place, placeDecoder)

-- MODEL


type alias AutocompleteAddress =
    { address : String
    , lat : Latitude
    , lng : Longitude
    }

type DeliveryTime
    = NoDrivers
    | Slow
    | Quick

type alias OrderInfo =
    { price : Int
    , deliveryTime : DeliveryTime
    , currency : String
    }

type Autocomplete
    = ErrorGettingAutocomplete
    | LoadingAutocomplete
    | LoadedAutocomplete (List AutocompleteAddress)

type Places
    = ErrorGettingPlaces
    | LoadingPlaces
    | LoadedPlaces (List Place)

type OrderInfoStatus
    = ErrorGettingOrderInfo
    | LoadingOrderInfo
    | LoadedOrderInfo OrderInfo

type alias Model =
    { session : Session
    , token : String
    , destinationAutocomplete : Maybe Autocomplete
    , destinationInput : String
    , destinationFriendlyNameInput : String
    , selectedAutocomplete : Maybe AutocompleteAddress
    , destination : Maybe Place
    , places : Maybe Places
    , selectedPlace : Maybe Place
    , orderInfo : Maybe OrderInfoStatus
    , map : MapPosition
    }


-- MSG


type Msg
    = GotPlaces (Result Http.Error (List Place))
    | GotAutocomplete (Result Http.Error (List AutocompleteAddress))
    | GotOrderInfo (Result Http.Error OrderInfo)
    | CreatedDestinationPlace Place (Result Http.Error Int)
    | PlaceHovered Place
    | PlaceClicked Place
    | DestinationTextChange String
    | DestinationFriendlyNameChange String
    | DestinationSearchClicked
    | AutocompleteClicked AutocompleteAddress
    | AutocompleteResetClicked
    | ConfirmDestinationClicked
    | EditDestinationClicked
    | EditPlaceClicked
    | CreateOrderPressed
    | OrderCreated (Result Http.Error ())

-- INIT

init : Session -> ( Model, Cmd Msg )
init session =
    let
        model token =
            Model session token Nothing "" "" Nothing Nothing Nothing Nothing Nothing defaultMapPosition
    in
    case session.auth of
        LoggedIn token user ->
            if user.isClient then
                case session.pendingEvent of
                    OrderInProgress ->
                        ( model token, Nav.pushUrl session.key <| Routes.toString <| Routes.Tracker )
                    _ ->
                        ( model token, Cmd.none )
            else
                ( model "", Nav.pushUrl session.key "/")
        Anonymous ->
            let
                thisUrl = Routes.toString Routes.Order |> Just
            in
            ( model "", Nav.pushUrl session.key (Routes.toString <| Routes.Authentication thisUrl) )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlaces result ->
            case result of
                Ok places ->
                    ( { model | places = Just (LoadedPlaces places) }, Cmd.none )
                Err _ ->
                    ( { model | places = Just ErrorGettingPlaces }, Cmd.none )

        GotAutocomplete result ->
            case result of
                Ok autocomplete ->
                    ( { model | destinationAutocomplete = Just (LoadedAutocomplete autocomplete) }, Cmd.none )
                Err _ ->
                    ( { model | destinationAutocomplete = Just ErrorGettingAutocomplete }, Cmd.none )

        GotOrderInfo result ->
            case result of
                Ok orderInfo ->
                    ( { model | orderInfo = Just (LoadedOrderInfo orderInfo) }, Cmd.none )
                Err _ ->
                    ( { model | orderInfo = Just ErrorGettingOrderInfo }, Cmd.none )

        PlaceHovered place ->
            let
                oldMap = model.map
                newMap = { oldMap | latitude = place.lat, longitude = place.lng, zoom = 15 }
            in
            ( { model | map = newMap }, Cmd.none )

        PlaceClicked place ->
            case model.destination of
                Just dst ->
                    ( { model | selectedPlace = Just place, orderInfo = Just LoadingOrderInfo }
                    , performGetOrderInfo model.session.api model.token dst place
                    )
                Nothing -> ( model, Cmd.none )

        DestinationTextChange text ->
            ( { model | destinationInput = text }, Cmd.none )

        DestinationFriendlyNameChange text ->
            ( { model | destinationFriendlyNameInput = text }, Cmd.none )

        DestinationSearchClicked ->
            ( { model | destinationAutocomplete = Just LoadingAutocomplete }, performGetAutocomplete model.session.api model.token model.destinationInput )

        AutocompleteClicked address ->
            let
                oldMap = model.map
                newMap = { oldMap | latitude = address.lat, longitude = address.lng, zoom = 15 }
            in
            ( { model | map = newMap, selectedAutocomplete = Just address, destinationInput = "", destinationAutocomplete = Nothing }, Cmd.none )

        AutocompleteResetClicked ->
            ( { model | selectedAutocomplete = Nothing, selectedPlace = Nothing, orderInfo = Nothing }, Cmd.none )

        ConfirmDestinationClicked ->
            case model.selectedAutocomplete of
                Just selection ->
                    let
                        destination = Place 1  model.destinationFriendlyNameInput "" selection.address selection.lat selection.lng
                    in
                    ( model, performCreatePlace model.session.api model.token destination )
                Nothing ->
                    ( model, Cmd.none )

        CreatedDestinationPlace place result ->
            case result of
                Ok id ->
                    let
                        destination = { place | id = id }
                    in
                    ( { model | destination = Just destination, places = Just LoadingPlaces }
                    , performGetPlaces model.session.api model.token destination
                    )
                Err _ ->
                    ( model, Cmd.none )

        EditDestinationClicked ->
            ( { model | destination = Nothing }, Cmd.none )

        EditPlaceClicked ->
            case model.destination of
                Just dst -> ( { model | selectedPlace = Nothing }, performGetPlaces model.session.api model.token dst )
                Nothing -> ( { model | selectedPlace = Nothing, places = Nothing }, Cmd.none )

        OrderCreated result ->
            case result of
                Ok _ ->
                    let
                        oldSession = model.session
                        newSession = { oldSession | pendingEvent = OrderInProgress }
                    in
                    ( { model | session = newSession }, Routes.toString Routes.Tracker |> Nav.pushUrl model.session.key )
                _ ->
                    ( model, Cmd.none )

        CreateOrderPressed ->
            case ( model.selectedPlace, model.destination ) of
                ( Just from, Just to ) ->
                    ( model, performCreateOrder model.session.api model.token from to )
                ( _, _ ) ->
                    ( model, Cmd.none )


-- HTTP

placesDecoder : Decoder.Decoder (List Place)
placesDecoder =
    placeDecoder |> Decoder.list

autocompleteAddressDecoder : Decoder.Decoder (List AutocompleteAddress)
autocompleteAddressDecoder =
    Decoder.map3 AutocompleteAddress
        (Decoder.field "address" Decoder.string)
        (Decoder.field "latitude" Types.latDecoder)
        (Decoder.field "longitude" Types.lngDecoder)
    |> Decoder.list

orderInfoDecoder : Decoder.Decoder OrderInfo
orderInfoDecoder =
    Decoder.map3 OrderInfo
        (Decoder.field "price" Decoder.int)
        (Decoder.at [ "deliveryTime", "case" ] (Decoder.string
            |> Decoder.andThen (\v ->
                if v == "Quick" then
                    Decoder.succeed Quick
                else if v == "Slow" then
                    Decoder.succeed Slow
                else if v == "NoDrivers" then
                    Decoder.succeed NoDrivers
                else
                    Decoder.fail "Unknown case for deliveryTime")))
        (Decoder.field "currency" Decoder.string)



performGetPlaces : String -> String -> Place -> Cmd Msg
performGetPlaces api token selection =
    let
        url = buildUrl api [ "order", "places" ]
            [ UrlBuilder.string "lat" (selection.lat |> Types.latToFloat |> String.fromFloat)
            , UrlBuilder.string "lng" (selection.lng |> Types.lngToFloat |> String.fromFloat)
            ]
    in
    Jwt.Http.get token
        { url = url
        , expect = Http.expectJson GotPlaces placesDecoder
        }

performGetAutocomplete : String -> String -> String -> Cmd Msg
performGetAutocomplete api token query =
    let
        url = buildUrl api [ "order", "autocomplete" ] [ UrlBuilder.string "q" query ]
    in
    Jwt.Http.get token
        { url = url
        , expect = Http.expectJson GotAutocomplete autocompleteAddressDecoder
        }

performGetOrderInfo : String -> String -> Place -> Place -> Cmd Msg
performGetOrderInfo api token destination place =
    let
        url = buildUrl api [ "order", "info" ]
            [ UrlBuilder.string "lat" (destination.lat |> Types.latToFloat |> String.fromFloat)
            , UrlBuilder.string "lng" (destination.lng |> Types.lngToFloat |> String.fromFloat)
            , UrlBuilder.int "placeId" place.id
            ]
    in
    Jwt.Http.get token
        { url = url
        , expect = Http.expectJson GotOrderInfo orderInfoDecoder
        }

performCreatePlace : String -> String -> Place -> Cmd Msg
performCreatePlace api token place =
    let
        url = buildUrl api [ "order", "createPlace" ] []
        args = Encoder.object
            [ ( "name", Encoder.string place.name )
            , ( "description", Encoder.string place.additionalDescription )
            , ( "latitude", Types.latToFloat place.lat |> Encoder.float )
            , ( "longitude", Types.lngToFloat place.lng |> Encoder.float )
            ]
        decoder =
            Decoder.field "id" Decoder.int
    in
    Jwt.Http.post token
        { url = url
        , expect = Http.expectJson (CreatedDestinationPlace place) decoder
        , body = Http.jsonBody args
        }

performCreateOrder : String -> String -> Place -> Place -> Cmd Msg
performCreateOrder api token from to =
    let
        url = buildUrl api [ "order", "create" ] []
        args = Encoder.object
            [ ( "fromId", Encoder.int from.id )
            , ( "toId", Encoder.int to.id )
            ]
    in
    Jwt.Http.post token
        { url = url
        , expect = Http.expectWhatever OrderCreated
        , body = Http.jsonBody args
        }


-- VIEW


view : Device -> Model -> ( String, Element Msg )
view device model =
    let
        content =
            Styles.responsiveMapContentView device model viewSelector viewMap
    in
    ( translate model.session.language "Place an order", content )

viewMap : Model -> Element Msg
viewMap model =
    let
        placeMarkers =
            case model.places of
                Just (LoadedPlaces places) ->
                    List.map (\p -> geomarker p.lng p.lat) places
                _ -> []

        allMarkers =
            case (model.destination, model.selectedPlace, model.selectedAutocomplete) of
                (Just dst, Just place, _) ->
                    [ geomarker dst.lng dst.lat
                    , geomarker place.lng place.lat
                    ]
                (Just dst, Nothing, _) ->
                    geomarker dst.lng dst.lat :: placeMarkers
                (Nothing, Nothing, Just autocomplete) ->
                    [ geomarker autocomplete.lng autocomplete.lat ]
                (Nothing, Nothing, Nothing) ->
                    []
                (_, _, _) ->
                    placeMarkers
    in
    if model.session.isWebGL then
        geomap [ width fill, height fill ]
            [ Map.position model.map ] allMarkers
    else
        el [ width fill, height fill ] <|
            el [ centerX, centerY ] <| paragraph [ padding 10 ] [ text <| translate model.session.language "Please enable WebGL support in your browser. See https://get.webgl.org/" ]


viewSelector : Model -> Element Msg
viewSelector model =
    case (model.destination, model.selectedPlace) of
        (Just destination, Just place) ->
            viewConfirmation model.session.language place destination model.orderInfo
        (Just destination, Nothing) ->
            viewPlaceList model.session.language destination model.places
        (Nothing, _) ->
            viewDestinationSelector model

viewPlaceInfo : String -> String -> Place -> Msg -> Element Msg
viewPlaceInfo lang infoText place onClick =
    row
        [ width fill
        , Border.widthEach { left = 0, top = 0, bottom = 1, right = 0 }
        , Border.color Styles.colorGrey
        , paddingXY 10 20
        ]
        [ column [ spacing 5 ]
            [ el [ Font.italic, Font.size 18, Font.light ] (text infoText)
            , el [] (text place.name)
            ]
        , Input.button [ alignRight, Font.color (rgba255 106 106 106 1) ]
            { onPress = Just onClick
            , label = text (translate lang "Edit")
            }
        ]

viewDestinationSelector : Model -> Element Msg
viewDestinationSelector model =
    let
        confirm =
            el [ paddingXY 25 0, width fill ] <|
                case model.selectedAutocomplete of
                    Just _ ->
                        Styles.largeButton ConfirmDestinationClicked
                            <| translate model.session.language "Confirm"
                    Nothing ->
                        Styles.largeButtonDisabled
                            (translate model.session.language "Confirm")
                            (translate model.session.language "Select a destination address to continue")

        search =
            case model.selectedAutocomplete of
                Nothing ->
                    row [ centerX, width fill, spacing 10, paddingXY 20 10 ]
                        [ Input.search [ width (fillPortion 9) ]
                            { onChange = DestinationTextChange
                            , text = model.destinationInput
                            , placeholder = Nothing
                            , label = Input.labelHidden (translate model.session.language "Delivery destination address input")
                            }
                        , el [ width (fillPortion 1) , height fill ]
                            <| Styles.smallButton DestinationSearchClicked
                            <| translate model.session.language "Search"
                        ]
                Just selected ->
                    row [ centerX, width fill, spacing 15, paddingXY 20 10 ]
                    [ column [ width (fillPortion 5), spacing 5 ]
                        [ Input.text [ width fill ]
                            { onChange = DestinationFriendlyNameChange
                            , text = model.destinationFriendlyNameInput
                            , placeholder = Input.placeholder [] (translate model.session.language "Name" |> text) |> Just
                            , label = Input.labelHidden (translate model.session.language "Delivary destination friendly name")
                            }
                        , el [ width fill , height fill, Font.italic ]
                            <| text selected.address
                        ]
                        , el [ width (fillPortion 1), height fill ]
                            <| Styles.smallButton AutocompleteResetClicked
                            <| translate model.session.language "Reset"
                    ]

        autocompleteItem item =
            Input.button
                [ paddingXY 20 20
                , Border.widthXY 0 1
                , Border.color Styles.colorGrey
                , width fill
                , pointer
                , mouseOver
                    [ Background.color (rgba255 206 206 206 0.5) ]
                ]
                { onPress = Just (AutocompleteClicked item)
                , label = text item.address
                }

        autocomplete =
            case model.destinationAutocomplete of
                Just (LoadedAutocomplete items) ->
                    if List.length items == 0 then
                        el [ centerX ] (text (translate model.session.language "Not found"))
                    else
                        column [ spacing 10, width fill ] <| List.map autocompleteItem items
                Just LoadingAutocomplete ->
                    el [ centerX, Font.italic ] <| text <| translate model.session.language "Loading..."
                Just ErrorGettingAutocomplete ->
                    el [ centerX ] <| paragraph [] [ text <| translate model.session.language "An error occured. Try refreshing the page" ]
                Nothing ->
                    none

    in
    column [ width fill, height fill, paddingXY 0 25 ]
        [ column [ width fill, height fill, spacing 10 ]
            [ el [ centerX, Font.size 25 ] (text (translate model.session.language "Select delivery destination"))
            , search
            , autocomplete
            ]
        , confirm
        ]

viewPlaceList : String -> Place -> Maybe Places -> Element Msg
viewPlaceList lang destination places =
    column [ width fill, height (px 0) ]
        [ viewPlaceInfo lang (translate lang "Delivery to:") destination EditDestinationClicked
        , case places of
            Just (LoadedPlaces placeList) ->
                if List.length placeList == 0 then
                    el [ padding 20, centerX ] (text (translate lang "No places found"))
                else
                    column
                        [ width fill
                        , height fill
                        , paddingXY 10 25, spacing 20
                        ]
                        <| List.map viewPlace placeList
            Just LoadingPlaces ->
                el [ Font.italic, padding 20, centerX ] <| text <| translate lang "Loading..."
            Just ErrorGettingPlaces ->
                paragraph [ padding 20, centerX ] [ text <| translate lang "An error occured. Try refreshing the page" ]
            Nothing ->
                el [ ] ( text (translate lang "Invalid state") )
        ]

viewPlace : Place -> Element Msg
viewPlace place =
    column
        [ width fill, spacing 10, padding 15
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
        , Events.onMouseEnter (PlaceHovered place)
        , Events.onClick (PlaceClicked place)
        ]
        [ el [ Font.semiBold, Font.size 22 ] (text place.name)
        , el [] (text place.additionalDescription)
        , el [ Font.italic, Font.size 18 ] (text place.address)
        ]

viewConfirmation : String -> Place -> Place -> Maybe OrderInfoStatus -> Element Msg
viewConfirmation lang fromPlace toPlace orderInfo =
    let
        deliveryTimeToString time =
            case time of
                NoDrivers -> translate lang "No drivers are currently available. Your order may take a long time to deliver"
                Slow -> translate lang "Your order may take up to 3 hours to deliver"
                Quick -> translate lang "Your order will take a maximum of 30 minutes to deliver"

        viewOrderInfo order =
            column [ width fill, alignBottom, spacing 20, padding 25 ]
                <| case order of
                    Just (LoadedOrderInfo info) -> 
                        [ el [] <| text <| translate lang "Delivery price:"
                            ++ " " ++ String.fromFloat (toFloat info.price / 100) ++ " " ++ info.currency
                        , paragraph [] [ deliveryTimeToString info.deliveryTime |> text ]
                        , Styles.largeButton CreateOrderPressed
                            <| translate lang "Confirm"
                        ]
                    Just LoadingOrderInfo ->
                        [ el [] <| text <| translate lang "Loading..."
                        ]
                    Just ErrorGettingOrderInfo -> [ paragraph [] [text <| translate lang "An error occured. Try refreshing your browser" ] ]
                    Nothing -> []
    in
    column
        [ width fill, height fill ]
        [ viewPlaceInfo lang (translate lang "Delivery to:") toPlace EditDestinationClicked
        , viewPlaceInfo lang (translate lang "Delivery from:") fromPlace EditPlaceClicked
        , viewOrderInfo orderInfo
        ]
