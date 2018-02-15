module Request.Merchant exposing (..)

import Json.Decode exposing (Decoder, string, int, float)
import Json.Decode.Pipeline exposing (decode, required)
import Request.Helpers exposing (authorisedGet, category, date)
import Data.Merchant exposing (..)


merchant : Decoder Merchant
merchant =
    decode Merchant
        |> required "address" address
        |> required "created" date
        |> required "id" string
        |> required "logo" string
        |> required "emoji" string
        |> required "name" string
        |> required "category" category


address : Decoder Address
address =
    decode Address
        |> required "address" string
        |> required "city" string
        |> required "country" string
        |> required "latitude" float
        |> required "longitude" float
        |> required "postcode" string
        |> required "region" string
        |> required "short_formatted" string
