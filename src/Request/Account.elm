module Request.Account exposing (accounts)

import Json.Decode exposing (Decoder, string, int, field, list, andThen, map, fail, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Data.Account exposing (..)


account : Decoder Account
account =
    field "type" string
        |> andThen
            (\accountType ->
                case accountType of
                    "uk_prepaid" ->
                        map Prepaid prepaidInfo

                    "uk_retail" ->
                        map Retail retailInfo

                    otherType ->
                        fail <| "Unknown account type: " ++ otherType
            )


prepaidInfo : Decoder PrepaidInfo
prepaidInfo =
    decode PrepaidInfo
        |> required "id" string
        |> required "created" string
        |> required "description" string


retailInfo : Decoder RetailInfo
retailInfo =
    decode RetailInfo
        |> required "id" string
        |> required "created" string
        |> required "description" string
        |> required "account_number" string
        |> required "sort_code" string


accounts : Decoder (List Account)
accounts =
    field "accounts" (list account)
