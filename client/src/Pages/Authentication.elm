module Pages.Authentication exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Session exposing (Session, AuthStatus(..), loggedIn)
import Styles
import Utils exposing (buildUrl)
import Json.Decode as Decoder
import Json.Encode as Encoder
import Http
import Ports.WebSocket as WebSocket
import Ports.Storage exposing (saveToken)
import Translations exposing (translate)

-- MODEL


type alias Model =
    { session : Session
    , login : LoginModel
    , register : RegisterModel
    , returnUrl : Maybe String
    }


type Error
    = InvalidEmail
    | MissingName
    | MissingSurname
    | MissingEmail
    | MissingPassword
    | MissingPhone
    | InvalidPhone
    | PasswordTooShort
    | RemoteError RemoteError

type RemoteError
    = UnknownUser
    | RequestFailed
    | BadPassword
    | EmailExists

type alias LoginModel =
    { email : String
    , password : String
    , errors : List Error
    }

type alias RegisterModel =
    { email : String
    , password : String
    , name : String
    , surname : String
    , phone : String
    , errors : List Error
    }



-- MSG


type Msg
    = Login LoginMsg
    | Register RegisterMsg
    | GotLoginResult (Result Error AuthResult)
    | GotRegisterResult (Result Error AuthResult)

type LoginMsg
    = LoginEmailChange String
    | LoginPasswordChange String
    | LoginClick

type RegisterMsg
    = RegisterEmailChange String
    | RegisterPasswordChange String
    | NameChange String
    | SurnameChange String
    | PhoneChange String
    | RegisterClick


-- INIT


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session returnUrl =
    let
        login =
            LoginModel "" "" []

        register =
            RegisterModel "" "" "" "" "" []
    in
    ( Model session login register returnUrl, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        saveSession response =
            saveToken response.token

        authenticateSocket response =
            WebSocket.Authenticate response.token
            |> WebSocket.send

        setLoggedInSession response =
            let
                oldSession = model.session
                newSession = { oldSession | auth = loggedIn response.token }
            in
            { model | session = newSession }

        performRedirect =
            case model.returnUrl of
                Just url ->
                    Nav.pushUrl model.session.key url
                Nothing ->
                    Nav.pushUrl model.session.key "/"

        onAuthentication response =
            ( setLoggedInSession response
            , Cmd.batch
                [ saveSession response
                , authenticateSocket response
                , performRedirect
                ]
            )
    in
    case msg of
        GotLoginResult result ->
            case result of
                Ok response ->
                    onAuthentication response
                Err err ->
                    let
                        oldModel = model.login
                        newModel = { oldModel | errors = [ err ] }
                    in
                    ( { model | login = newModel }, Cmd.none )
        GotRegisterResult result ->
            case result of
                Ok response ->
                    onAuthentication response
                Err err ->
                    let
                        oldModel = model.register
                        newModel = { oldModel | errors = [ err ] }
                    in
                    ( { model | register = newModel }, Cmd.none )
        Login loginMsg ->
            let
                ( newModel, newMsg ) = updateLogin model.session model.login loginMsg
            in
            ( { model | login = newModel }, newMsg )
        Register registerMsg ->
            let
                ( newModel, newMsg ) = updateRegister model.session model.register registerMsg
            in
            ( { model | register = newModel }, newMsg )


updateLogin : Session -> LoginModel -> LoginMsg -> (LoginModel, Cmd Msg)
updateLogin session model msg =
    case msg of
        LoginEmailChange newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        LoginPasswordChange newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        LoginClick ->
            let
                errors = validateLogin model
                loginCmd =
                    if List.length errors == 0 then
                        performLogin session.api model
                    else
                        Cmd.none
            in
            ( { model | errors = errors }, loginCmd )

updateRegister : Session -> RegisterModel -> RegisterMsg -> (RegisterModel, Cmd Msg)
updateRegister session model msg =
    case msg of
        RegisterEmailChange newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        RegisterPasswordChange newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        NameChange newName ->
            ( { model | name = newName }, Cmd.none )

        SurnameChange newSurname ->
            ( { model | surname = newSurname }, Cmd.none )

        PhoneChange newPhone ->
            ( { model | phone = newPhone }, Cmd.none )

        RegisterClick ->
            let
                errors = validateRegister model
                registerCmd =
                    if List.length errors == 0 then
                        performRegister session.api model
                    else
                        Cmd.none
            in
            ( { model | errors = errors }, registerCmd )


validateLogin : LoginModel -> List Error
validateLogin login =
    []
    ++
    (if String.length login.email == 0 then [ MissingEmail ] else [])
    ++
    (if String.length login.password == 0 then [ MissingPassword ] else [])
    ++
    (if String.length login.email > 0 && not (validEmail login.email) then [ InvalidEmail ] else [])

validateRegister : RegisterModel -> List Error
validateRegister register =
    []
    ++
    (if String.length register.email == 0 then [ MissingEmail ] else [])
    ++
    (if String.length register.email > 0 && not (validEmail register.email) then [ InvalidEmail ] else [])
    ++
    (if String.length register.password == 0 then [ MissingPassword ] else [])
    ++
    (if String.length register.password < 5 then [ PasswordTooShort ] else [])
    ++
    (if String.length register.name == 0 then [ MissingName ] else [])
    ++
    (if String.length register.surname == 0 then [ MissingSurname ] else [])
    ++
    (if String.length register.phone == 0 then [ MissingPhone ] else [])
    ++
    (if String.length register.phone > 0 && not (validPhone register.phone) then [ InvalidPhone ] else [])


validPhone : String -> Bool
validPhone phone =
    True --TODO

validEmail : String -> Bool
validEmail email =
    True --TODO


-- HTTP

loginEncoder : LoginModel -> Encoder.Value
loginEncoder model =
    Encoder.object
        [ ("email", Encoder.string model.email)
        , ("password", Encoder.string model.password)
        ]

registerEncoder : RegisterModel -> Encoder.Value
registerEncoder model =
    Encoder.object
        [ ("email", Encoder.string model.email)
        , ("password", Encoder.string model.password)
        , ("phoneNumber", Encoder.string model.phone)
        , ("name", Encoder.string model.name)
        , ("surname", Encoder.string model.surname)
        ]

type alias AuthResult =
    { token : String }

authDecoder : Decoder.Decoder AuthResult
authDecoder =
    Decoder.map AuthResult
        (Decoder.field "token" Decoder.string)

authErrorDecoder : Decoder.Decoder Error
authErrorDecoder =
    Decoder.field "case" Decoder.string
    |> Decoder.andThen (\v ->
        if v == "ValidationError" then
            Decoder.field "fields"
                (Decoder.index 0 (Decoder.field "case" Decoder.string))
                |> Decoder.andThen (\t ->
                    if t == "InvalidEmail" then
                        Decoder.succeed InvalidEmail
                    else if t == "InvalidPhone" then
                        Decoder.succeed InvalidPhone
                    else if t == "PasswordTooShort" then
                        Decoder.succeed PasswordTooShort
                    else if t == "UnknownUser" then
                        Decoder.succeed (RemoteError UnknownUser)
                    else if t == "BadPassword" then
                        Decoder.succeed (RemoteError BadPassword)
                    else if t == "EmailExists" then
                        Decoder.succeed (RemoteError EmailExists)
                    else
                        Decoder.succeed (RemoteError RequestFailed))
        else
            Decoder.succeed (RemoteError RequestFailed))

expectJsonWithError : (Result Error a -> msg) -> Decoder.Decoder a -> Http.Expect msg
expectJsonWithError toMsg resultDecoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    case Decoder.decodeString resultDecoder body of
                        Ok value -> Ok value
                        _ -> Err (RemoteError RequestFailed)
                Http.BadStatus_ _ body ->
                    case Decoder.decodeString authErrorDecoder body of
                        Ok value -> Err value
                        _ -> Err (RemoteError RequestFailed)
                _ -> Err (RemoteError RequestFailed)

performLogin : String -> LoginModel -> Cmd Msg
performLogin api args =
    let
        url = buildUrl api [ "auth", "login" ] []
    in
    Http.post
        { url = url
        , body = Http.jsonBody (loginEncoder args)
        , expect = expectJsonWithError GotLoginResult authDecoder
        }

performRegister : String -> RegisterModel -> Cmd Msg
performRegister api args =
    let
        url = buildUrl api [ "auth", "register" ] []
    in
    Http.post
        { url = url
        , body = Http.jsonBody (registerEncoder args)
        , expect = expectJsonWithError GotRegisterResult authDecoder
        }

-- VIEW

errorToString : String -> Error -> String
errorToString lang error =
    case error of
        InvalidEmail -> (translate lang "Invalid email")
        MissingName -> (translate lang "Name is required")
        MissingSurname -> (translate lang "Surname is required")
        MissingEmail -> (translate lang "Email is required")
        MissingPassword -> (translate lang "Password is required")
        MissingPhone -> (translate lang "Phone number is required")
        InvalidPhone -> (translate lang "Invalid phone number")
        PasswordTooShort -> (translate lang "Entered password is too short - enter at least 5 characters")
        RemoteError remoteError ->
            case remoteError of
                UnknownUser -> (translate lang "This user does not exist")
                RequestFailed -> (translate lang "Request to server has failed")
                BadPassword -> (translate lang "You have entered a bad password")
                EmailExists -> (translate lang "Another user already registered with this email address")

view : Device -> Model -> ( String, Element Msg )
view device model =
    let
        register = viewRegister model.session model.register |> List.map (Element.map Register)
        login = viewLogin model.session model.login |> List.map (Element.map Login)
        content =
            case device.class of
                Phone ->
                    column [ width fill, spacingXY 0 24, paddingXY 0 12 ]
                        [ row [ width fill, centerX, padding 12 ]
                            [ column [ width fill, alignTop, spacing 24 ] login ]
                        , row [ width fill, centerX, padding 12 ]
                            [ column [ width fill, alignTop, spacing 24 ] register ]
                        ]
                _ ->
                    row [ width fill, height fill ]
                        [ row [ width fill, centerX, spacing 48 ]
                            [ column [ width fill, alignTop, spacing 24 ]
                                [ column [ alignRight, spacing 24, width (fill |> maximum 500) ] login ]
                            , column [ width fill, alignTop, spacing 24 ]
                                [ column [ alignLeft, spacing 24, width (fill |> maximum 500) ] register ]
                            ]
                        ]
    in
    ( (translate model.session.language "Login or register"), content )

viewLogin : Session -> LoginModel -> List (Element LoginMsg)
viewLogin session model =
    [ el [ centerX, Font.size 24 ] (text (translate session.language "Login"))
    , Input.email [ Input.focusedOnLoad ]
        { onChange = \t -> LoginEmailChange t
        , text = model.email
        , placeholder = Input.placeholder [] (text (translate session.language "your@email.com")) |> Just
        , label = Input.labelAbove [] (text (translate session.language "Email address"))
        }
    , Input.currentPassword []
        { onChange = \t -> LoginPasswordChange t
        , text = model.password
        , placeholder = Input.placeholder [] (text (translate session.language "********")) |> Just
        , label = Input.labelAbove [] (text (translate session.language "Password"))
        , show = False
        }
    , viewActionButton session (translate session.language "Login") LoginClick model.errors
    ]

viewRegister : Session -> RegisterModel -> List (Element RegisterMsg)
viewRegister session model =
    [ el [ centerX, Font.size 24 ] (text (translate session.language "Register"))
    , row [ spacing 12 ]
        [ Input.text []
            { onChange = \t -> NameChange t
            , text = model.name
            , placeholder = Nothing
            , label = Input.labelAbove [] (text (translate session.language "Name"))
            }
        , Input.text []
            { onChange = \t -> SurnameChange t
            , text = model.surname
            , placeholder = Nothing
            , label = Input.labelAbove [] (text (translate session.language "Surname"))
            }
        ]
    , Input.email []
        { onChange = \t -> RegisterEmailChange t
        , text = model.email
        , placeholder = Input.placeholder [] (text (translate session.language "your@email.com")) |> Just
        , label = Input.labelAbove [] (text (translate session.language "Email address"))
        }
    , Input.text []
        { onChange = \t -> PhoneChange t
        , text = model.phone
        , placeholder = Input.placeholder [] (text (translate session.language "(+48) 123 456 789")) |> Just
        , label = Input.labelAbove [] (text (translate session.language "Phone number"))
        }
    , Input.newPassword []
        { onChange = \t -> RegisterPasswordChange t
        , text = model.password
        , placeholder = Input.placeholder [] (text (translate session.language "********")) |> Just
        , label = Input.labelAbove [] (text (translate session.language "Password"))
        , show = False
        }
    , viewActionButton session (translate session.language "Register") RegisterClick model.errors
    ]

viewActionButton : Session -> String -> msg -> List Error -> Element msg
viewActionButton session title onClick errors =
    let
        errorsMap = \e -> el [ Font.color (rgba 1 0 0 1) ] <| text <| errorToString session.language e
        errorsDisplays = List.map errorsMap errors
    in
    el [ below (column [ alignLeft, spacing 6, paddingXY 0 12 ] errorsDisplays) , width fill , height fill ]
        <| Styles.largeButton onClick title
