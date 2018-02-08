module Data.Transaction exposing (..)

import Date exposing (Date)
import Date.Format exposing (format)
import FormatNumber exposing (formatFloat, usLocale)
import Data.Balance exposing (Currency, currencySymbol)


type alias Transaction =
    { id : String
    , amount : Int
    , currency : Currency
    , created : Date
    , merchantId : Maybe String
    , category : Category
    , isTopUp : Bool
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


formatAmount : Transaction -> String
formatAmount tx =
    formatFloat usLocale (toFloat (abs tx.amount) / 100)
        |> (++) (currencySymbol tx.currency)


formatDate : Transaction -> String
formatDate tx =
    format "%d %b %y" tx.created
