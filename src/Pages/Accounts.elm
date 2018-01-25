module Pages.Accounts exposing (..)

import Html exposing (..)
import Http
import Data.Account exposing (Account(..))
import Data.Balance exposing (Balance, Currency(..))
import Request.Account
import Request.Balance
import Request.Helpers exposing (authorisedGet)


type Accounts
    = Loading
    | Error
    | Accounts (List Account)


type alias Model =
    { balance : Maybe Balance
    , accounts : Accounts
    }


type Msg
    = RefreshAccounts
    | ShowAccounts (Result Http.Error (List Account))
    | ShowBalance (Result Http.Error Balance)


init : Model
init =
    Model Nothing Loading


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshAccounts ->
            ( model, loadAccounts )

        ShowAccounts (Ok accounts) ->
            ( { model | accounts = Accounts accounts }
            , loadBalance <|
                List.head <|
                    List.filter
                        (\acc ->
                            case acc of
                                Retail _ ->
                                    True

                                _ ->
                                    False
                        )
                        accounts
            )

        ShowAccounts (Err _) ->
            ( { model | accounts = Error }, Cmd.none )

        ShowBalance (Ok balance) ->
            ( { model | balance = Just balance }, Cmd.none )

        ShowBalance (Err _) ->
            ( model, Cmd.none )


loadAccounts : Cmd Msg
loadAccounts =
    let
        request =
            authorisedGet "accounts" Request.Account.accounts
    in
        Http.send ShowAccounts request


loadBalance : Maybe Account -> Cmd Msg
loadBalance account =
    case account of
        Just account_ ->
            let
                request =
                    case account_ of
                        Prepaid prepaidInfo ->
                            authorisedGet ("balance?account_id=" ++ prepaidInfo.id) Request.Balance.balance

                        Retail retailInfo ->
                            authorisedGet ("balance?account_id=" ++ retailInfo.id) Request.Balance.balance
            in
                Http.send ShowBalance request

        Nothing ->
            Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ text <|
                case model.accounts of
                    Loading ->
                        "Loading Accounts"

                    Accounts accounts ->
                        toString accounts

                    Error ->
                        "Something went wrong!"
            ]
        , p []
            [ text <|
                case model.balance of
                    Just balance ->
                        toString balance

                    Nothing ->
                        "Loading Balance"
            ]
        ]
