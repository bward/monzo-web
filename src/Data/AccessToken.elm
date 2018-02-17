module Data.AccessToken exposing (..)


type alias AccessToken =
    { token : String
    , clientId : String
    , expiresIn : Int
    , tokenType : String
    , userId : String
    }
