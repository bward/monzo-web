module Pages.Accounts exposing (..)

import Html exposing (..)
import Http
import Data.Account exposing (Account)
import Request.Account
import Request.Helpers exposing (authorisedGet)


type Model
    = Loading
    | Error
    | Accounts (List Account)


type Msg
    = RefreshAccounts
    | ShowAccounts (Result Http.Error (List Account))


init : Model
init =
    Loading


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshAccounts ->
            ( model, loadAccounts )

        ShowAccounts (Ok accounts) ->
            ( Accounts accounts, Cmd.none )

        ShowAccounts (Err error) ->
            ( Error, Cmd.none )


loadAccounts : Cmd Msg
loadAccounts =
    let
        request =
            authorisedGet "accounts" Request.Account.accountsResponse
    in
        Http.send ShowAccounts request


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ text <|
                case model of
                    Loading ->
                        "Loading"

                    Accounts accounts ->
                        toString accounts

                    Error ->
                        "Something went wrong!"
            ]
        ]
