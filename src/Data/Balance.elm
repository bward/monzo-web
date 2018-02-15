module Data.Balance exposing (..)

import FormatNumber exposing (formatFloat, usLocale)


type alias Balance =
    { balance : Int
    , currency : Currency
    , spendToday : Int
    }


type Currency
    = GBP


format : (Balance -> Int) -> Balance -> String
format selector bal=
    formatFloat usLocale (toFloat (selector bal) / 100)
        |> (++) (currencySymbol bal.currency)


currencySymbol : Currency -> String
currencySymbol cur =
    case cur of
        GBP ->
            "£"
