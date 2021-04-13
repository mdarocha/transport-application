module Flags exposing (Flags, fromJson)

import Json.Decode as Decoder exposing (bool, int, string, nullable, field, index, errorToString)
import Json.Encode as Encoder

type alias Flags =
    { isWebGL : Bool
    , isOnline : Bool
    , isWebSocket : Bool
    , apiUrl : String
    , window : (Int, Int)
    , userToken : Maybe String
    , language : String
    }


decoder : Decoder.Decoder Flags
decoder =
    Decoder.map7 Flags
        (field "isWebGL" bool)
        (field "isOnline" bool)
        (field "isWebSocket" bool)
        (field "apiUrl" string)
        (field "window" (Decoder.map2 Tuple.pair
            (index 0 int)
            (index 1 int)))
        (field "userToken" (nullable string))
        (field "language" string)


fromJson : Encoder.Value -> Flags
fromJson value =
    case Decoder.decodeValue decoder value of
        Ok data ->
            data
        Err err ->
            Flags False False False (errorToString err) (0, 0) Nothing "en"
