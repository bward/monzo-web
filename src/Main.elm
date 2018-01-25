module Main exposing (..)

import Html exposing (..)
import Pages.Accounts as Accounts


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Page
    = AccountsPage Accounts.Model


type alias Model =
    { page : Page }


init : ( Model, Cmd Msg )
init =
    ( { page = AccountsPage Accounts.init }, Cmd.map AccountsMsg Accounts.loadAccounts )


type Msg
    = AccountsMsg Accounts.Msg


view : Model -> Html Msg
view model =
    case model.page of
        AccountsPage subModel ->
            Html.map AccountsMsg (Accounts.view subModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( AccountsMsg subMsg, AccountsPage subModel ) ->
            let
                ( newModel, newCmd ) =
                    Accounts.update subMsg subModel
            in
                ( { model | page = AccountsPage newModel }, Cmd.map AccountsMsg newCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
