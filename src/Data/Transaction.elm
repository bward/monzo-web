module Data.Transaction exposing (..)

import Date exposing (Date)
import FormatNumber exposing (formatFloat, usLocale)
import Data.Balance exposing (Currency, currencySymbol)


type alias Transaction =
    { id : String
    , amount : Int
    , currency : Currency
    , created : Date
    , merchantId : Maybe String
    , category : Category
    , isLoad : Bool
    , settled : Maybe Date
    , declineReason : Maybe DeclineReason
    }


type DeclineReason
    = InsufficientFunds
    | CardInactive
    | CardBlocked
    | Other


type Category
    = Monzo
    | General
    | EatingOut
    | Expenses
    | Transport
    | Cash
    | Bills
    | Entertainment
    | Shopping
    | Holidays
    | Groceries


format : Transaction -> String
format tx =
    formatFloat usLocale (toFloat (abs tx.amount) / 100)
        |> (++) (currencySymbol tx.currency)
