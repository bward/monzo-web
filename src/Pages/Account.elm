module Pages.Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Task
import Data.Account
import Data.Balance exposing (Balance)
import Data.Transaction exposing (Transaction)
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
    { accounts : Account
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
            ( { model | accounts = Account ( acc, bal ) }, loadTransactions acc )

        ShowAccount (Err _) ->
            ( { model | accounts = Error }, Cmd.none )

        ShowTransactions (Ok txs) ->
            ( { model | transactions = txs }, Cmd.none )

        ShowTransactions (Err e) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ id "account" ]
            [ case model.accounts of
                Loading ->
                    p [] [ text "Loading Accounts" ]

                Account accounts ->
                    renderAccount accounts

                Error ->
                    p [] [ text "Something went wrong!" ]
            ]
        , div [ id "transactions" ]
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
    div [ class "transaction" ]
        [ span [] [ text <| (Data.Transaction.format tx) ++ " on " ++ (toString tx.created) ]
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
