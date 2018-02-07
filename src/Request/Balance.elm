module Request.Balance exposing (addBalance)

import Json.Decode exposing (Decoder, string, int, andThen, map, fail, succeed, andThen)
import Json.Decode.Pipeline exposing (decode, required)
import Task
import Http
import Request.Helpers exposing (authorisedGet)
import Data.Account exposing (..)
import Data.Balance exposing (..)


balance : Decoder Balance
balance =
    decode Balance
        |> required "balance" int
        |> required "currency" currency
        |> required "spend_today" int


currency : Decoder Currency
currency =
    string
        |> andThen
            (\currencyString ->
                case currencyString of
                    "GBP" ->
                        succeed GBP

                    _ ->
                        fail "Unrecognised currency"
            )


getBalance : Account -> Task.Task Http.Error Balance
getBalance acc =
    authorisedGet ("balance?account_id=" ++ acc.id) balance
        |> Http.toTask


addBalance : Account -> Task.Task Http.Error ( Account, Balance )
addBalance acc =
    getBalance acc
        |> Task.map (\bal -> ( acc, bal ))
