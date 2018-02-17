module Request.Account exposing (getAccount)

import Json.Decode exposing (Decoder, string, int, field, index, andThen, map, fail, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Http
import Task
import Request.Helpers exposing (authorisedGet)
import Data.AccessToken exposing (..)
import Data.Account exposing (..)


account : Decoder Account
account =
    field "type" string
        |> andThen
            (\accountType ->
                case accountType of
                    "uk_retail" ->
                        accountInfo

                    otherType ->
                        fail <| "Unknown account type: " ++ otherType
            )


accountInfo : Decoder Account
accountInfo =
    decode Account
        |> required "id" string
        |> required "created" string
        |> required "description" string
        |> required "account_number" string
        |> required "sort_code" string


accounts : Decoder Account
accounts =
    field "accounts" (index 0 account)


getAccount : AccessToken -> Task.Task Http.Error Account
getAccount token =
    authorisedGet token "accounts?account_type=uk_retail" accounts
        |> Http.toTask
