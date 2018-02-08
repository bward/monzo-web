module Request.Transaction exposing (getTransactions)

import Json.Decode exposing (Decoder, string, int, bool, field, list, andThen, map, nullable, fail, succeed, maybe)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Date exposing (Date)
import Task
import Http
import Data.Account exposing (..)
import Data.Transaction exposing (..)
import Request.Balance exposing (currency)
import Request.Helpers exposing (authorisedGet)


getTransactions : Account -> Task.Task Http.Error (List Transaction)
getTransactions acc =
    authorisedGet ("transactions?account_id=" ++ acc.id) transactions
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
        |> required "merchant" (nullable string)
        |> required "category" category
        |> required "amount" isTopUp
        |> optional "settled" (maybe date) Nothing
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


date : Decoder Date
date =
    string
        |> map Date.fromString
        |> andThen
            (\res ->
                case res of
                    Ok d ->
                        succeed d

                    Err e ->
                        fail e
            )


category : Decoder Category
category =
    string
        |> andThen
            (\cat ->
                case cat of
                    "mondo" ->
                        succeed Monzo

                    "general" ->
                        succeed General

                    "eating_out" ->
                        succeed EatingOut

                    "expenses" ->
                        succeed Expenses

                    "transport" ->
                        succeed Transport

                    "cash" ->
                        succeed Cash

                    "bills" ->
                        succeed Bills

                    "entertainment" ->
                        succeed Entertainment

                    "shopping" ->
                        succeed Shopping

                    "holidays" ->
                        succeed Holidays

                    "groceries" ->
                        succeed Groceries

                    _ ->
                        fail ("Unknown category" ++ cat)
            )


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
