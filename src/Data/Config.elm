module Data.Config exposing (..)


type alias Config =
    { clientId: String
    , clientSecret: String
    , redirectUri: String
    }