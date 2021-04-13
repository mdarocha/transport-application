module Ports.GeoMap exposing (MapPosition, defaultMapPosition, geomap, geomarker, onPositionChange, position)

import Element exposing (..)
import Html exposing (node)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Types exposing (Longitude(..), Latitude(..))

type alias MapPosition =
    { longitude : Longitude
    , latitude : Latitude
    , zoom : Float
    , rotation : Float
    , pitch : Float
    }

defaultMapPosition =
    MapPosition (Longitude 19.9368564) (Latitude 50.0619474) 11 0 0

type MapAttribute msg
    = OnPositionChange (Html.Attribute msg)
    | CustomPosition MapPosition
    | MapStyle String


mapAttributeToHtml : MapAttribute msg -> List (Html.Attribute msg)
mapAttributeToHtml mapAttribute =
    case mapAttribute of
        OnPositionChange event ->
            [ event ]

        CustomPosition pos ->
            [ attribute "lng" (pos.longitude |> Types.lngToFloat |> String.fromFloat)
            , attribute "lat" (pos.latitude |> Types.latToFloat |> String.fromFloat)
            , attribute "zoom" (String.fromFloat pos.zoom)
            , attribute "rotate" (String.fromFloat pos.rotation)
            , attribute "pitch" (String.fromFloat pos.pitch)
            ]

        MapStyle style ->
            [ attribute "map-style" style ]


mapPositionDecoder : Decoder MapPosition
mapPositionDecoder =
    Decode.map5 MapPosition
        (Decode.at [ "detail", "longitude" ] Types.lngDecoder)
        (Decode.at [ "detail", "latitude" ] Types.latDecoder)
        (Decode.at [ "detail", "zoom" ] Decode.float)
        (Decode.at [ "detail", "rotation" ] Decode.float)
        (Decode.at [ "detail", "pitch" ] Decode.float)


onPositionChange : (MapPosition -> msg) -> MapAttribute msg
onPositionChange updateMsg =
    mapPositionDecoder
        |> Decode.map updateMsg
        |> on "map-position-change"
        |> OnPositionChange


position : MapPosition -> MapAttribute msg
position pos =
    CustomPosition pos

type alias MapMarker =
    { longitude : Float
    , latitude : Float
    }

geomarker : Longitude -> Latitude -> MapMarker
geomarker (Longitude lng) (Latitude lat) =
    MapMarker lng lat

geomap : List (Attribute msg) -> List (MapAttribute msg) -> List MapMarker -> Element msg
geomap elAttributes mapAttributes geoMarkers =
    let
        htmlAttributes =
            List.map mapAttributeToHtml mapAttributes
                |> List.concat
                |> List.append
                    [ style "display" "flex"
                    , style "flex-grow" "100000"
                    , style "min-width" "100px"
                    , style "min-height" "100px"
                    ]
        markers =
            geoMarkers |> List.map (\m ->
                node "geo-marker"
                    [ attribute "lng" (String.fromFloat m.longitude)
                    , attribute "lat" (String.fromFloat m.latitude)
                    ] [])
    in
    el elAttributes <|
        html <|
            node "geo-map" htmlAttributes markers
