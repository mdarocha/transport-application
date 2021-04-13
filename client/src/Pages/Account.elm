module Pages.Account exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Session exposing (Session, AuthStatus(..))
import Translations exposing (translate)
import Styles
import Ports.Storage as Storage
import Browser.Navigation as Nav
import Routes

-- MODEL


type alias Model =
    { session : Session
    , token : String
    }


-- MSG

type Msg
    = LogoutClicked


-- INIT

init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session "", Cmd.none )



-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogoutClicked ->
            let
                oldSession = model.session
                newSession = { oldSession | auth = Anonymous }
            in
            ( { model | session = newSession }
            , Cmd.batch
                [ Storage.saveToken ""
                , Nav.pushUrl newSession.key
                    <| Routes.toString Routes.Root
                ]
            )



-- VIEW


view : Device -> Model -> ( String, Element Msg )
view device model =
    let
        content =
            row [ width fill, height fill ]
            [ el [ width (fillPortion 1), height fill ]
                <| viewUserProfile model
            , el [ width (fillPortion 3), height fill ]
                <| viewOrderHistory model
            ]
    in
    ( (translate model.session.language "Account"), content )

viewUserProfile : Model -> Element Msg
viewUserProfile model =
    column [ width fill, height fill ]
    [ el [ width fill, alignBottom, padding 20 ]
        <| Styles.largeButton LogoutClicked
        <| translate model.session.language "Logout"
    ]

viewOrderHistory : Model -> Element Msg
viewOrderHistory model =
    el [] (text "order history")
