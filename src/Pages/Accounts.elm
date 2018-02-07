module Pages.Accounts exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import List
import Task
import Data.Account exposing (Account(..))
import Data.Balance exposing (Balance, Currency(..))
import Request.Account
import Request.Balance
import Request.Helpers exposing (authorisedGet)


type Accounts
    = Loading
    | Error
    | Accounts (List ( Account, Balance ))


type alias Model =
    { accounts : Accounts
    }


type Msg
    = RefreshAccounts
    | ShowAccounts (Result Http.Error (List ( Account, Balance )))


init : Model
init =
    Model Loading


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshAccounts ->
            ( model, loadAccounts )

        ShowAccounts (Ok accounts) ->
            ( { model | accounts = Accounts accounts }, Cmd.none )

        ShowAccounts (Err _) ->
            ( { model | accounts = Error }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "accounts" ]
        (case model.accounts of
            Loading ->
                [ p [] [ text "Loading Accounts" ] ]

            Accounts accounts ->
                List.map renderAccount accounts

            Error ->
                [ p [] [ text "Something went wrong!" ] ]
        )


renderAccount : ( Account, Balance ) -> Html Msg
renderAccount ( acc, bal ) =
    case acc of
        Retail info ->
            div [ class "account retail-account" ]
                [ span [] [ text "Current account" ]
                , span [ class "account-number" ] [ text info.number ]
                , span [ class "sort-code" ] [ text info.sortCode ]
                , span [ class "balance" ] [ text <| Data.Balance.format bal ]
                ]

        Prepaid info ->
            div [ class "account prepaid-account" ]
                [ span [] [ text "Prepaid account" ]
                , span [ class "balance" ] [ text <| Data.Balance.format bal ]
                ]


loadAccounts : Cmd Msg
loadAccounts =
    let
        getAccounts =
            authorisedGet "accounts" Request.Account.accounts
                |> Http.toTask

        addBalance acc =
            loadBalance acc
                |> Task.map (\bal -> ( acc, bal ))

        getAccountsAndBalances =
            getAccounts
                |> Task.andThen
                    (\accs ->
                        Task.succeed (List.sortBy Data.Account.compare accs)
                    )
                |> Task.andThen
                    (\accs ->
                        Task.sequence (List.map addBalance accs)
                    )
    in
        Task.attempt ShowAccounts getAccountsAndBalances


loadBalance : Account -> Task.Task Http.Error Balance
loadBalance account =
    let
        request =
            case account of
                Prepaid prepaidInfo ->
                    authorisedGet ("balance?account_id=" ++ prepaidInfo.id) Request.Balance.balance

                Retail retailInfo ->
                    authorisedGet ("balance?account_id=" ++ retailInfo.id) Request.Balance.balance
    in
        Http.toTask request
