module Request.Balance exposing (addBalance, currency)

import Json.Decode exposing (Decoder, string, int, andThen, map, fail, succeed, andThen)
import Json.Decode.Pipeline exposing (decode, required)
import Task
import Http
import Request.Helpers exposing (authorisedGet)
import Data.AccessToken exposing (..)
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
            (\cur ->
                case cur of
                    "GBP" ->
                        succeed GBP

                    _ ->
                        fail ("Unrecognised currency" ++ cur)
            )


getBalance : AccessToken -> Account -> Task.Task Http.Error Balance
getBalance token acc =
    authorisedGet token ("balance?account_id=" ++ acc.id) balance 
        |> Http.toTask


addBalance : AccessToken -> Account -> Task.Task Http.Error ( Account, Balance )
addBalance token acc =
    getBalance token acc 
        |> Task.map (\bal -> ( acc, bal ))
