port module Ports.Geolocation exposing (start, stop, update, Error(..), GeoPosition)

import Json.Decode as Decode
    exposing (Decoder, field, at, float, int, string, andThen, succeed, fail, errorToString)
import Time exposing (Posix)
import Types exposing (Longitude(..), Latitude(..))

port geolocationStart: () -> Cmd msg
port geolocationStop: () -> Cmd msg
port geolocationUpdate: (Decode.Value -> msg) -> Sub msg

type Error
    = PermissionDenied
    | PositionUnavailable
    | Timeout
    | UnknownError String

type alias GeoPosition =
    { timestamp : Posix
    , lat : Latitude
    , lng : Longitude
    , accuracy : Float
    , heading : Maybe Float
    }

decoder : Decoder (Result Error GeoPosition)
decoder =
    field "msgType" string
    |> andThen (\t ->
        if t == "Error" then
            field "errorCode" int
            |> andThen (\e ->
                if e == 1 then
                    succeed <| Err PermissionDenied
                else if e == 2 then
                    succeed <| Err PositionUnavailable
                else if e == 3 then
                    succeed <| Err Timeout
                else
                    fail "unknown error code")
        else if t == "Update" then
            Decode.map5 GeoPosition
                (field "timestamp" int
                    |> andThen (\time -> succeed <| Time.millisToPosix time))
                (at [ "coords", "latitude" ] Types.latDecoder)
                (at [ "coords", "longitude" ] Types.lngDecoder)
                (at [ "coords", "accuracy" ] float)
                (at [ "coords", "heading" ] (Decode.nullable float))
            |> andThen (\p -> succeed <| Ok p)
        else
            fail "unknown type")

update : (Result Error GeoPosition -> msg) -> Sub msg
update toMsg =
    geolocationUpdate (\v ->
        (case Decode.decodeValue decoder v of
            Ok res -> res
            Err err -> Err <| UnknownError ("geolocation - json decode error " ++ errorToString err))
        |> toMsg)

start: () -> Cmd msg
start = geolocationStart

stop: () -> Cmd msg
stop = geolocationStop
