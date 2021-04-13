module Session exposing (Session, AuthStatus(..), WebSocketStatus(..), PendingEventStatus(..), User, loggedIn, fromFlags)

import Flags exposing (Flags)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (field, string, andThen)
import Jwt exposing (decodeToken)
import Bool.Extra exposing (fromString)


type AuthStatus
    = LoggedIn String User
    | Anonymous

type alias User =
    { sub : String
    , name : String
    , surname : String
    , email : String
    , isClient : Bool
    , isDriver : Bool
    , isOwner : Bool
    }

type WebSocketStatus
    = Closed Int
    | Open
    | Authenticated

type PendingEventStatus
    = NoEvent
    | OrderInProgress
    | JobInProgress

type alias Session =
    { api : String
    , isWebGL : Bool
    , key : Nav.Key
    , auth : AuthStatus
    , isOnline : Bool
    , wsStatus : WebSocketStatus
    , window : (Int, Int)
    , headerMenuExpanded : Bool
    , language : String
    , pendingEvent : PendingEventStatus
    }

stringAsBoolDecoder : Decode.Decoder Bool
stringAsBoolDecoder =
    string |> andThen (\val ->
        case fromString val of
            Just v ->
                Decode.succeed v
            Nothing ->
                Decode.fail "Cant decode as bool")

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map7 User
        (field "sub" string)
        (field "name" string)
        (field "surname" string)
        (field "email" string)
        (field "is_client" stringAsBoolDecoder)
        (field "is_driver" stringAsBoolDecoder)
        (field "is_owner" stringAsBoolDecoder)

loggedIn : String -> AuthStatus
loggedIn token =
    case decodeToken userDecoder token of
        Ok user ->
            LoggedIn token user
        Err _ ->
            Anonymous

fromFlags : Flags -> Nav.Key -> Session
fromFlags flags key =
    let
        user =
            case flags.userToken of
                Nothing -> Anonymous
                Just token -> loggedIn token
    in
    Session flags.apiUrl flags.isWebGL key user flags.isOnline (Closed 0) flags.window False flags.language NoEvent
