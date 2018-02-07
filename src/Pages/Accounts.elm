module Pages.Accounts exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Task
import Data.Account
import Data.Balance
import Request.Account exposing (getAccount)
import Request.Balance exposing (addBalance)


type alias AccountInfo =
    ( Data.Account.Account, Data.Balance.Balance )


type Account
    = Loading
    | Error
    | Account AccountInfo


type alias Model =
    { accounts : Account
    }


type Msg
    = RefreshAccounts
    | ShowAccounts (Result Http.Error AccountInfo)


init : Model
init =
    Model Loading


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshAccounts ->
            ( model, loadAccount )

        ShowAccounts (Ok account) ->
            ( { model | accounts = Account account }, Cmd.none )

        ShowAccounts (Err _) ->
            ( { model | accounts = Error }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "accounts" ]
        [ case model.accounts of
            Loading ->
                p [] [ text "Loading Accounts" ]

            Account accounts ->
                renderAccount accounts

            Error ->
                p [] [ text "Something went wrong!" ]
        ]


renderAccount : AccountInfo -> Html Msg
renderAccount ( acc, bal ) =
    div [ class "account retail-account" ]
        [ div [] [ text "Current account" ]
        , div [ class "account-number" ] [ text acc.number ]
        , div [ class "sort-code" ] [ text acc.sortCode ]
        , div [ class "balance" ] [ text (Data.Balance.format bal) ]
        ]


loadAccount : Cmd Msg
loadAccount =
    getAccount
        |> Task.andThen addBalance
        |> Task.attempt ShowAccounts
