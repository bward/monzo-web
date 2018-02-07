module Request.Helpers exposing (authorisedGet)

import Http
import Json.Decode exposing (Decoder, string, andThen, succeed, fail)
import Config


authorisedGet : String -> Decoder a -> Http.Request a
authorisedGet url jsonDecoder =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Config.accessToken) ]
        , url = "https://api.monzo.com/" ++ url
        , body = Http.emptyBody
        , expect = Http.expectJson jsonDecoder
        , timeout = Nothing
        , withCredentials = False
        }
