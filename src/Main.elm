module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Task
import RemoteData exposing (WebData, RemoteData(..))
import Data.AccessToken exposing (AccessToken)
import Data.Account
import Data.Balance exposing (Balance)
import Data.Config exposing (Config)
import Data.Transaction exposing (Transaction)
import Request.Account exposing (getAccount)
import Request.Balance exposing (addBalance)
import Request.Transaction exposing (getTransactions)
import Request.AccessToken exposing (exchangeAuthorizationCode)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


type alias Flags =
    { config : Config
    , authorizationCode : String
    }


type alias Model =
    { config : Config
    , accessToken : WebData AccessToken
    , account : WebData AccountInfo
    , transactions : WebData (List Transaction)
    , detailTransactionId : Maybe String
    , page : Int
    , transactionsPerPage : Int
    }


type alias AccountInfo =
    ( Data.Account.Account, Balance )


type Msg
    = LoadAccount (Result Http.Error AccessToken)
    | ShowAccount (Result Http.Error AccountInfo)
    | ShowTransactions (Result Http.Error (List Transaction))
    | DetailTransaction (Maybe String)
    | Forward
    | Backward


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { config = flags.config
      , accessToken = Loading
      , account = NotAsked
      , transactions = NotAsked
      , detailTransactionId = Nothing
      , page = 0
      , transactionsPerPage = 30
      }
    , exchangeCode flags.config flags.authorizationCode
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadAccount (Ok token) ->
            ( { model | accessToken = Success token }, loadAccount token )

        LoadAccount (Err e) ->
            ( { model | accessToken = Failure e }, Cmd.none )

        ShowAccount (Ok ( acc, bal )) ->
            case model.accessToken of
                Success token ->
                    ( { model | account = Success ( acc, bal ), transactions = Loading }, loadTransactions token acc )

                _ ->
                    ( model, Cmd.none )

        ShowAccount (Err e) ->
            ( { model | account = Failure e }, Cmd.none )

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

        Forward ->
            ( { model | page = model.page - 1 }, Cmd.none )

        Backward ->
            ( { model | page = model.page + 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "wrapper" ]
        [ div [ id "header" ] [ h1 [] [ text "💰💰💰" ] ]
        , div [ id "today" ]
            (case model.account of
                Success ( acc, bal ) ->
                    [ h2 [] [ text "Today" ]
                    , h1
                        [ class
                            (if bal.spendToday < 0 then
                                "expense"
                             else
                                "income"
                            )
                        ]
                        [ text (Data.Balance.format (\b -> abs b.spendToday) bal) ]
                    ]

                Failure _ ->
                    [ text "" ]

                _ ->
                    loader
            )
        , div [ id "account" ]
            (case model.account of
                Success accounts ->
                    renderAccount accounts

                Failure _ ->
                    [ p [] [ text "Something went wrong!" ] ]

                _ ->
                    loader
            )
        , div [ id "top-controls" ] (renderControls model)
        , table [ id "transactions" ]
            (case model.transactions of
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
                        (txs
                            |> List.drop (model.page * model.transactionsPerPage)
                            |> List.take model.transactionsPerPage
                        )

                Failure _ ->
                    [ tr [] [ td [] [] ] ]

                _ ->
                    [ tr [] [ td [] loader ] ]
            )
        , div [ id "stats" ] loader
        , div [ id "bottom-controls" ] (renderControls model)
        ]


exchangeCode : Config -> String -> Cmd Msg
exchangeCode config code =
    exchangeAuthorizationCode config code
        |> Http.send LoadAccount


loader : List (Html Msg)
loader =
    [ div [ class "loader" ] [] ]


renderControls : Model -> List (Html Msg)
renderControls model =
    [ if model.page > 0 then
        button [ onClick Forward ] [ text "⟵" ]
      else
        text ""
    , div [ class "spacer" ] []
    , let
        maxPages =
            case model.transactions of
                Success txs ->
                    (List.length txs) // 30

                _ ->
                    0
      in
        if model.page < maxPages then
            button [ onClick Backward ] [ text "⟶" ]
        else
            text ""
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
    tr [ onClick (DetailTransaction (Just tx.id)) ]
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


loadAccount : AccessToken -> Cmd Msg
loadAccount accessToken =
    getAccount accessToken
        |> Task.andThen (addBalance accessToken)
        |> Task.attempt ShowAccount


loadTransactions : AccessToken -> Data.Account.Account -> Cmd Msg
loadTransactions accessToken acc =
    getTransactions accessToken acc
        |> Task.attempt ShowTransactions
