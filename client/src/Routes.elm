module Routes exposing (Route(..), fromUrl, toString)

import Dict exposing (Dict)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, query)
import Url.Parser.Query as Query

type Route
    = Tracker
    | Authentication (Maybe String)
    | Account
    | Order
    | Jobs
    | OrderResult
    | Root


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root Parser.top
        , Parser.map Tracker (s "tracker")
        , Parser.map Authentication (s "login" <?> Query.string "redirect")
        , Parser.map Account (s "account")
        , Parser.map Order (s "order")
        , Parser.map Jobs (s "jobs")
        , Parser.map OrderResult (s "result")
        ]

fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url

toString : Route -> String
toString route =
    let
        ( paths, params ) = routeToPieces route
    in
    Builder.absolute paths params


routeToPieces : Route -> ( List String, List Builder.QueryParameter )
routeToPieces route =
    case route of
        Tracker -> ( [ "tracker" ], [] )
        Authentication url ->
            let
                query = case url of
                    Just existingUrl -> [ Builder.string "redirect" existingUrl ]
                    Nothing -> []
            in
            ( [ "login" ], query )
        Account -> ( [ "account" ], [] )
        Order -> ( [ "order" ], [] )
        Jobs -> ( [ "jobs" ], [] )
        OrderResult -> ( [ "result" ], [] )
        Root -> ( [], [] )
