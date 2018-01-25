module Request.Balance exposing (balance)

import Json.Decode exposing (Decoder, string, int, andThen, map, fail, succeed, andThen)
import Json.Decode.Pipeline exposing (decode, required)
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
