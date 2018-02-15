module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Task
import RemoteData exposing (WebData, RemoteData(..))
import Data.Account
import Data.Balance exposing (Balance)
import Data.Transaction exposing (Transaction)
import Request.Account exposing (getAccount)
import Request.Balance exposing (addBalance)
import Request.Transaction exposing (getTransactions)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { account : WebData AccountInfo
    , transactions : WebData (List Transaction)
    , detailTransactionId : Maybe String
    }


type alias AccountInfo =
    ( Data.Account.Account, Balance )


type Msg
    = RefreshAccount
    | ShowAccount (Result Http.Error AccountInfo)
    | ShowTransactions (Result Http.Error (List Transaction))
    | DetailTransaction (Maybe String)


init : ( Model, Cmd Msg )
init =
    ( { account = Loading
      , transactions = NotAsked
      , detailTransactionId = Nothing
      }
    , loadAccount
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshAccount ->
            ( model, loadAccount )

        ShowAccount (Ok ( acc, bal )) ->
            ( { model | account = Success ( acc, bal ), transactions = Loading }, loadTransactions acc )

        ShowAccount (Err err) ->
            ( { model | account = Failure err }, Cmd.none )

        ShowTransactions (Ok txs) ->
            ( { model | transactions = Success txs }, Cmd.none )

        ShowTransactions (Err e) ->
            ( model, Cmd.none )

        DetailTransaction txId ->
            let
                oldId =
                    model.detailTransactionId

                newId =
                    if txId == oldId then
                        Nothing
                    else
                        txId
            in
                ( { model | detailTransactionId = newId }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "wrapper" ]
        [ div [ id "header" ] [ h1 [] [ text "💰💰💰" ] ]
        , div [ id "today" ]
            (case model.account of
                NotAsked ->
                    loader

                Loading ->
                    loader

                Success ( acc, bal ) ->
                    [ h2 [] [ text "Today" ], h1 [] [ text (Data.Balance.format (\b -> b.spendToday) bal) ] ]

                Failure _ ->
                    [ text "" ]
            )
        , div [ id "account" ]
            (case model.account of
                NotAsked ->
                    loader

                Loading ->
                    loader

                Success accounts ->
                    renderAccount accounts

                Failure _ ->
                    [ p [] [ text "Something went wrong!" ] ]
            )
        , div [ id "top-controls" ] renderControls
        , table [ id "transactions" ]
            (case model.transactions of
                NotAsked ->
                    [ tr [] [ td [] loader ] ]

                Loading ->
                    [ tr [] [ td [] loader ] ]

                Success txs ->
                    List.concatMap
                        (\tx ->
                            [ renderTransaction tx
                            , if model.detailTransactionId == Just tx.id then
                                renderDetailedTransaction tx
                              else
                                text ""
                            ]
                        )
                        txs

                Failure _ ->
                    [ tr [] [ td [] [] ] ]
            )
        , div [ id "stats" ] loader
        , div [ id "bottom-controls" ] renderControls
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


loader : List (Html Msg)
loader =
    [ div [ class "loader" ] [] ]


renderControls : List (Html Msg)
renderControls =
    [ button [] [ text "Forward" ]
    , div [class "spacer"] []
    , button [] [ text "Backward" ]
    ]


renderAccount : AccountInfo -> List (Html Msg)
renderAccount ( acc, bal ) =
    [ div [] [ text "Current account" ]
    , div [ class "account-number" ] [ text acc.number ]
    , div [ class "sort-code" ] [ text acc.sortCode ]
    , div [ class "balance" ] [ text (Data.Balance.format (\b -> b.balance) bal) ]
    ]


renderTransaction : Transaction -> Html Msg
renderTransaction tx =
    tr [ class "transaction", onClick (DetailTransaction (Just tx.id)) ]
        [ td [ class "created" ] [ text (Data.Transaction.formatDate tx) ]
        , td [ class "merchant" ] [ text (Data.Transaction.formatDescription tx) ]
        , td [ class "category" ] [ text (toString tx.category) ]
        , td [ class "income" ]
            [ text <|
                if tx.isTopUp then
                    (Data.Transaction.formatAmount tx)
                else
                    ""
            ]
        , td [ class "expense" ]
            [ text <|
                if tx.isTopUp then
                    ""
                else
                    (Data.Transaction.formatAmount tx)
            ]
        ]


renderDetailedTransaction : Transaction -> Html Msg
renderDetailedTransaction tx =
    tr [ rowspan 3, onClick (DetailTransaction (Just tx.id)) ]
        [ td [ colspan 5 ]
            [ text
                ((Data.Transaction.formatTime tx)
                    ++ (case tx.merchant of
                            Just m ->
                                " at " ++ m.address.formatted

                            Nothing ->
                                ""
                       )
                )
            ]
        ]


loadAccount : Cmd Msg
loadAccount =
    getAccount
        |> Task.andThen addBalance
        |> Task.attempt ShowAccount


loadTransactions : Data.Account.Account -> Cmd Msg
loadTransactions acc =
    getTransactions acc
        |> Task.attempt ShowTransactions
