module Request.Helpers exposing (authorisedGet, stringToInt)

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


stringToInt : Decoder Int
stringToInt =
    string
        |> andThen
            (\parsedString ->
                case String.toInt parsedString of
                    Err _ ->
                        fail <| "Can't parse " ++ parsedString ++ " as a number"

                    Ok val ->
                        succeed val
            )
