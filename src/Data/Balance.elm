module Data.Balance exposing (..)


type alias Balance =
    { balance : Int
    , currency : Currency
    , spendToday : Int
    }


type Currency
    = GBP
