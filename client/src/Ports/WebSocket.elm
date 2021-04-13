port module Ports.WebSocket exposing
    ( open
    , openAfterDelay
    , opened
    , closed
    , wentOffline
    , wentOnline
    , send
    , received
    , WebSocketMessage(..)
    )

import Json.Encode as Encode
import Json.Decode as Decode
import Types exposing (Latitude(..), Longitude(..), latDecoder, lngDecoder, OrderStatus, orderStatusDecoder)
import Time exposing (Posix)


port open: () -> Cmd msg
port openAfterDelay : Float -> Cmd msg

port sendMessage: Encode.Value -> Cmd msg
port receivedMessage : (Decode.Value -> msg) -> Sub msg

port opened : (() -> msg) -> Sub msg
port closed : (() -> msg) -> Sub msg

port wentOffline : (() -> msg) -> Sub msg
port wentOnline : (() -> msg) -> Sub msg

type WebSocketMessage
    = Authenticate String
    | AuthenticationResult Bool
    | LocationUpdate Latitude Longitude Posix
    | RefreshJobs
    | OrderStatusUpdate OrderStatus

send: WebSocketMessage -> Cmd msg
send message =
    let
        (msgType, msgData) =
            case message of
                Authenticate token ->
                    ("Authenticate"
                    , Encode.object
                        [ ( "token", Encode.string token ) ]
                    )
                AuthenticationResult success ->
                    ("AuthenticationResult"
                    , Encode.object
                        [ ( "success", Encode.bool success ) ]
                    )

                LocationUpdate (Latitude lat) (Longitude lng) time ->
                    ("LocationUpdate"
                    , Encode.object
                        [ ( "lat", Encode.float lat )
                        , ( "lng", Encode.float lng )
                        , ( "time", Time.posixToMillis time |> Encode.int )
                        ]
                    )

                RefreshJobs ->
                    ("RefreshJobs"
                    , Encode.object []
                    )

                OrderStatusUpdate status ->
                    ("OrderStatusUpdate"
                    , Encode.object [] --TODO
                    )

        encodedMessage =
            Encode.object
                [ ( "Type", Encode.string msgType )
                , ( "Data", msgData )
                ]
    in
    sendMessage encodedMessage

decodeMessage : Decode.Decoder WebSocketMessage
decodeMessage =
    Decode.field "Type" Decode.string
        |> Decode.andThen (\t ->
            case t of
                "AuthenticationResult" ->
                    Decode.map AuthenticationResult
                        (Decode.at [ "Data", "success" ] Decode.bool)
                "Authenticate" ->
                    Decode.map Authenticate
                        (Decode.at [ "Data", "token" ] Decode.string)

                "LocationUpdate" ->
                    Decode.map3 LocationUpdate
                        (Decode.at [ "Data", "lat" ] latDecoder)
                        (Decode.at [ "Data", "lng" ] lngDecoder)
                        (Decode.at [ "Data", "time"] Decode.int
                            |> Decode.andThen (\v -> Time.millisToPosix v |> Decode.succeed))

                "RefreshJobs" ->
                    Decode.succeed RefreshJobs

                "OrderStatusUpdate" ->
                    Decode.map OrderStatusUpdate
                        (Decode.at [ "Data", "status" ] orderStatusDecoder)

                _ -> Decode.fail "Unknown message type!"
        )

decodeReceived : Decode.Value -> Maybe WebSocketMessage
decodeReceived msg =
    case Decode.decodeValue decodeMessage msg of
        Ok res -> Just res
        Err _ -> Nothing

received : (Maybe WebSocketMessage -> msg) -> Sub msg
received toMsg =
    receivedMessage (\v -> decodeReceived v |> toMsg)
