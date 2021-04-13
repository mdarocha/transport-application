module Pages.NotFound exposing (view)

import Element exposing (..)
import Element.Font as Font
import Translations exposing (translate)
import Session exposing (Session)

view : Device -> Session -> ( String, Element msg )
view _ session =
    let
        content =
            column [ width fill, height fill ]
                [ el
                    [ centerX
                    , centerY
                    , Font.size 48
                    ]
                    (text (translate session.language "Page not found"))
                ]
    in
    ( translate session.language "Not Found", content )
