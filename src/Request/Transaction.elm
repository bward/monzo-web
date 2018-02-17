module Request.Transaction exposing (getTransactions)

import Json.Decode exposing (Decoder, string, int, field, list, andThen, map, nullable, fail, succeed, maybe)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Task
import Http
import Data.Account exposing (..)
import Data.Transaction exposing (..)
import Data.AccessToken exposing (..)
import Request.Balance exposing (currency)
import Request.Merchant exposing (merchant)
import Request.Helpers exposing (authorisedGet, date, category)


getTransactions : AccessToken -> Account -> Task.Task Http.Error (List Transaction)
getTransactions token acc =
    authorisedGet token ("transactions?expand[]=merchant&account_id=" ++ acc.id) transactions 
        |> Http.toTask
        |> Task.map List.reverse


transactions : Decoder (List Transaction)
transactions =
    field "transactions" (list transaction)


transaction : Decoder Transaction
transaction =
    decode Transaction
        |> required "id" string
        |> required "amount" int
        |> required "currency" currency
        |> required "created" date
        |> required "merchant" (nullable merchant)
        |> required "category" category
        |> required "amount" isTopUp
        |> optional "settled" (maybe date) Nothing
        |> optional "counterparty" (maybe counterparty) Nothing
        |> optional "decline_reason" (maybe declineReason) Nothing


isTopUp : Decoder Bool
isTopUp =
    int
        |> andThen
            (\amount ->
                if amount > 0 then
                    succeed True
                else
                    succeed False
            )


counterparty : Decoder Counterparty
counterparty =
    decode Counterparty
        |> required "name" string
        |> required "user_id" string


declineReason : Decoder DeclineReason
declineReason =
    string
        |> andThen
            (\dec ->
                case dec of
                    "INSUFFICIENT_FUNDS" ->
                        succeed InsufficientFunds

                    "CARD_INACTIVE" ->
                        succeed CardInactive

                    "CARD_BLOCKED" ->
                        succeed CardBlocked

                    "OTHER" ->
                        succeed Other

                    _ ->
                        fail ("Unknown reason" ++ dec)
            )
