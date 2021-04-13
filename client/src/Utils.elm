module Utils exposing (buildUrl)

import Url.Builder exposing (absolute, QueryParameter)


buildUrl : String -> List String -> List QueryParameter -> String
buildUrl root path params =
    let
        url = absolute path params
    in
    String.join "" [ root, url ]

