module Pages.Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Task
import Data.Account
import Data.Balance exposing (Balance)
import Data.Transaction exposing (Transaction)
import Data.Merchant
import Request.Account exposing (getAccount)
import Request.Balance exposing (addBalance)
import Request.Transaction exposing (getTransactions)


type alias AccountInfo =
    ( Data.Account.Account, Balance )


type Account
    = Loading
    | Error
    | Account AccountInfo


type alias Model =
    { account : Account
    , transactions : List Transaction
    }


type Msg
    = RefreshAccount
    | ShowAccount (Result Http.Error AccountInfo)
    | ShowTransactions (Result Http.Error (List Transaction))


init : Model
init =
    Model Loading []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshAccount ->
            ( model, loadAccount )

        ShowAccount (Ok ( acc, bal )) ->
            ( { model | account = Account ( acc, bal ) }, loadTransactions acc )

        ShowAccount (Err _) ->
            ( { model | account = Error }, Cmd.none )

        ShowTransactions (Ok txs) ->
            ( { model | transactions = txs }, Cmd.none )

        ShowTransactions (Err e) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "wrapper" ]
        [ div [ id "account" ]
            [ case model.account of
                Loading ->
                    p [] [ text "Loading Accounts" ]

                Account accounts ->
                    renderAccount accounts

                Error ->
                    p [] [ text "Something went wrong!" ]
            ]
        , table [ id "transactions" ]
            (List.map renderTransaction model.transactions)
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
