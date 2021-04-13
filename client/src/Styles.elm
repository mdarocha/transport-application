module Styles exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Element.Background as Background
import Element.Region as Region
import Element.Font as Font

colorBrand =
    rgba255 102 106 209 1

colorAccent =
    rgba255 209 206 102 1

colorGrey =
    rgba255 206 206 206 1

colorError =
    rgba255 255 0 0 1

responsiveMapContentView : Device -> model -> (model -> Element msg) -> (model -> Element msg) -> Element msg
responsiveMapContentView device model viewContent viewMap =
    case device.class of
        Phone ->
            column [ width fill, height fill ]
                [ el [ height (fillPortion 1), width fill ]
                    <| viewMap model
                , el [ scrollbars, height (fillPortion 3), width fill ]
                    <| viewContent model
                ]

        Tablet ->
            row [ width fill, height fill ]
                [ el [ scrollbars, height fill, width (fillPortion 1) ]
                    <| viewContent model
                , el [ height fill, width (fillPortion 1) ]
                    <| viewMap model
                ]

        _ ->
            row [ width fill, height fill ]
                [ el [ scrollbars, height fill, width (fillPortion 2) ]
                    <| viewContent model
                , el [ height fill, width (fillPortion 4) ]
                    <| viewMap model
                ]

largeButtonDisabled : String -> String -> Element msg
largeButtonDisabled label description =
    Input.button
        [ width fill, height fill
        , Background.color (rgba255 209 206 102 0.5), paddingXY 10 20
        , Region.description description
        ]
        { onPress = Nothing
        , label = el [ centerX ] (text label)
        }

largeButtonColored : Color -> msg -> String -> Element msg
largeButtonColored color onClick label =
    Input.button
        [ width fill, height fill
        , Background.color color
        , paddingXY 10 20
        ]
        { onPress = Just onClick
        , label = el [ centerX ] (text label)
        }

largeButton : msg -> String -> Element msg
largeButton =
    largeButtonColored colorAccent


smallButton : msg -> String -> Element msg
smallButton onClick label =
    Input.button
        [ Background.color colorAccent
        , padding 10
        , Border.rounded 5
        , height fill
        , width fill
        ]
        { onPress = Just onClick
        , label = el [ centerX ] (text label)
        }

faIcon : String -> Element msg
faIcon icon =
    el
        [ Font.family [ Font.typeface "FontAwesome" ]
        , Font.unitalicized
        ]
    <| text icon
