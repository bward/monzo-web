module Data.Merchant exposing (..)

import Date exposing (Date)
import Data.Category exposing (Category)


type alias Merchant =
    { address : Address
    , created : Date
    , id : String
    , logo : String
    , emoji : String
    , name : String
    , category : Category
    }


type alias Address =
    { address : String
    , city : String
    , country : String
    , latitude : Float
    , longitude : Float
    , postcode : String
    , region : String
    }


formatName : Maybe Merchant -> String
formatName mabes =
    case mabes of
        Just merch ->
            merch.name ++ " " ++ merch.emoji

        Nothing ->
            ""
