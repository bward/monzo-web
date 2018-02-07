module Data.Account exposing (..)


type Account
    = Prepaid PrepaidInfo
    | Retail RetailInfo


type alias PrepaidInfo =
    { id : String
    , created : String
    , description : String
    }


type alias RetailInfo =
    { id : String
    , created : String
    , description : String
    , number : String
    , sortCode : String
    }


compare : Account -> Int
compare acc =
    case acc of
        Retail _ ->
            0

        Prepaid _ ->
            1
