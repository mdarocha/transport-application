module Types exposing
    ( Latitude(..)
    , Longitude(..)
    , latToFloat
    , lngToFloat
    , latDecoder
    , lngDecoder
    , Place
    , placeDecoder
    , OrderStatus(..)
    , orderStatusDecoder
    )

import Json.Decode as Decode exposing (field, int, string, succeed, float, andThen, Decoder)


-- Lat, lng custom types

type Latitude
    = Latitude Float

type Longitude
    = Longitude Float


latToFloat : Latitude -> Float
latToFloat (Latitude lat) = lat

lngToFloat : Longitude -> Float
lngToFloat (Longitude lng) = lng

latDecoder : Decoder Latitude
latDecoder =
    float |>
    andThen (\v -> Latitude v |> succeed)

lngDecoder : Decoder Longitude
lngDecoder =
    float |>
    andThen (\v -> Longitude v |> succeed)


-- Place type

type alias Place =
    { id : Int
    , name : String
    , additionalDescription : String
    , address : String
    , lat : Latitude
    , lng : Longitude
    }

placeDecoder : Decoder Place
placeDecoder =
    Decode.map6 Place
        (field "id" int)
        (field "friendlyName" string)
        (field "additionalDescription" string)
        (field "address" string)
        (field "latitude" latDecoder)
        (field "longitude" lngDecoder)


-- Order types

type OrderStatus
    = SearchingForDriver
    | Delivering
    | Completed
    | Failed

orderStatusDecoder : Decoder OrderStatus
orderStatusDecoder =
    Decode.int
    |> Decode.andThen (\v ->
        case v of
            0 -> Decode.succeed SearchingForDriver
            1 -> Decode.succeed Delivering
            2 -> Decode.succeed Completed
            3 -> Decode.succeed Failed
            _ -> Decode.fail "Unknown order status!")
