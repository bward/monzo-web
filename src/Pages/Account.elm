module Pages.Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Task
import RemoteData exposing (WebData, RemoteData(..))
import Data.Account
import Data.Balance exposing (Balance)
import Data.Transaction exposing (Transaction)
import Request.Account exposing (getAccount)
import Request.Balance exposing (addBalance)
import Request.Transaction exposing (getTransactions)


type alias AccountInfo =
    ( Data.Account.Account, Balance )


type alias Model =
    { account : WebData AccountInfo
    , transactions : WebData (List Transaction)
    }


type Msg
    = RefreshAccount
    | ShowAccount (Result Http.Error AccountInfo)
    | ShowTransactions (Result Http.Error (List Transaction))


init : Model
init =
    { account = Loading
    , transactions = NotAsked
    }


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


view : Model -> Html Msg
view model =
    div [ id "wrapper" ]
        [ div [ id "account" ]
            [ case model.account of
                NotAsked ->
                    div [ class "loader" ] []

                Loading ->
                    div [ class "loader" ] []

                Success accounts ->
                    renderAccount accounts

                Failure _ ->
                    p [] [ text "Something went wrong!" ]
            ]
        , table [ id "transactions" ]
            (case model.transactions of
                NotAsked ->
                    [ tr [] [ td [] [] ] ]

                Loading ->
                    [ tr [] [ td [] [ div [ class "loader" ] [] ] ] ]

                Success txs ->
                    List.map renderTransaction txs

                Failure _ ->
                    [ tr [] [ td [] [] ] ]
            )
        ]


renderAccount : AccountInfo -> Html Msg
renderAccount ( acc, bal ) =
    div [ class "account retail-account" ]
        [ div [] [ text "Current account" ]
        , div [ class "account-number" ] [ text acc.number ]
        , div [ class "sort-code" ] [ text acc.sortCode ]
        , div [ class "balance" ] [ text (Data.Balance.format bal) ]
        ]


renderTransaction : Transaction -> Html Msg
renderTransaction tx =
    tr [ class "transaction" ]
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


loadAccount : Cmd Msg
loadAccount =
    getAccount
        |> Task.andThen addBalance
        |> Task.attempt ShowAccount


loadTransactions : Data.Account.Account -> Cmd Msg
loadTransactions acc =
    getTransactions acc
        |> Task.attempt ShowTransactions
