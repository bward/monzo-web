module Data.Transaction exposing (..)

import Date exposing (Date)
import Date.Format exposing (format)
import FormatNumber exposing (formatFloat, usLocale)
import Data.Balance exposing (Currency, currencySymbol)
import Data.Category exposing (Category)
import Data.Merchant exposing (Merchant)


type alias Transaction =
    { id : String
    , amount : Int
    , currency : Currency
    , created : Date
    , merchant : Maybe Merchant
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


formatAmount : Transaction -> String
formatAmount tx =
    formatFloat usLocale (toFloat (abs tx.amount) / 100)
        |> (++) (currencySymbol tx.currency)


formatDate : Transaction -> String
formatDate tx =
    format "%d %b %y" tx.created
