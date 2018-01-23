module Decoders exposing (accountsResponse)

import Json.Decode exposing (Decoder, string, int, field, list, andThen, map, fail, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Account exposing (..)


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
        |> required "account_number" stringToInt
        |> required "sort_code" stringToInt


accountsResponse : Decoder (List Account)
accountsResponse =
    field "accounts" (list account)


stringToInt : Decoder Int
stringToInt =
    string
        |> andThen
            (\parsedString ->
                case String.toInt parsedString of
                    Err _ ->
                        fail <| "Can't parse " ++ parsedString ++ " as a number"

                    Ok val ->
                        succeed val
            )
