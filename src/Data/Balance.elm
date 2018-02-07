module Data.Balance exposing (..)

import FormatNumber exposing (formatFloat, usLocale)


type alias Balance =
    { balance : Int
    , currency : Currency
    , spendToday : Int
    }


type Currency
    = GBP


format : Balance -> String
format bal =
    formatFloat usLocale (toFloat bal.balance / 100)
        |> (++) (currencySymbol bal.currency)


currencySymbol : Currency -> String
currencySymbol cur =
    case cur of
        GBP ->
            "£"
