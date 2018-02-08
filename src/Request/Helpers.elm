module Request.Helpers exposing (..)

import Http
import Json.Decode exposing (Decoder, string, andThen, succeed, fail, map)
import Date exposing (Date)
import Data.Category exposing (Category(..))
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


date : Decoder Date
date =
    string
        |> map Date.fromString
        |> andThen
            (\res ->
                case res of
                    Ok d ->
                        succeed d

                    Err e ->
                        fail e
            )


category : Decoder Category
category =
    string
        |> andThen
            (\cat ->
                case cat of
                    "mondo" ->
                        succeed Monzo

                    "general" ->
                        succeed General

                    "eating_out" ->
                        succeed EatingOut

                    "expenses" ->
                        succeed Expenses

                    "transport" ->
                        succeed Transport

                    "cash" ->
                        succeed Cash

                    "bills" ->
                        succeed Bills

                    "entertainment" ->
                        succeed Entertainment

                    "shopping" ->
                        succeed Shopping

                    "holidays" ->
                        succeed Holidays

                    "groceries" ->
                        succeed Groceries

                    _ ->
                        fail ("Unknown category" ++ cat)
            )
