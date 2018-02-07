module Main exposing (..)

import Html exposing (..)
import Pages.Account as Account


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Page
    = AccountsPage Account.Model


type alias Model =
    { page : Page }


init : ( Model, Cmd Msg )
init =
    ( { page = AccountsPage Account.init }, Cmd.map AccountsMsg Account.loadAccount )


type Msg
    = AccountsMsg Account.Msg


view : Model -> Html Msg
view model =
    case model.page of
        AccountsPage subModel ->
            Html.map AccountsMsg (Account.view subModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( AccountsMsg subMsg, AccountsPage subModel ) ->
            let
                ( newModel, newCmd ) =
                    Account.update subMsg subModel
            in
                ( { model | page = AccountsPage newModel }, Cmd.map AccountsMsg newCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
