module Account exposing (..)


type alias PrepaidInfo =
    { id : String
    , created : String
    , description : String
    }


type alias RetailInfo =
    { id : String
    , created : String
    , description : String
    , number : Int
    , sortCode : Int
    }


type Account
    = Prepaid PrepaidInfo
    | Retail RetailInfo
