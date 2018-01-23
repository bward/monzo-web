module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http
import Decoders
import Account exposing (..)
import Config


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { accessToken : String
    , response : String
    }


init : ( Model, Cmd Msg )
init =
    ( { accessToken = Config.accessToken
      , response = "Loading..."
      }
    , loadAccounts Config.accessToken
    )


type Msg
    = LoadAccounts
    | ShowAccounts (Result Http.Error (List Account))


update msg model =
    case msg of
        LoadAccounts ->
            ( model, loadAccounts model.accessToken )

        ShowAccounts (Ok accounts) ->
            ( { model | response = toString accounts }, Cmd.none )

        ShowAccounts (Err error) ->
            case error of
                Http.BadStatus response ->
                    ( { model | response = response.body }, Cmd.none )

                _ ->
                    ( { model | response = "Something went wrong!" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "monzo-web" ]
        , p [] [ text model.response ]
        , button [ onClick LoadAccounts ] [ text "load" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


loadAccounts : String -> Cmd Msg
loadAccounts apiKey =
    let
        url =
            "https://api.monzo.com/accounts"

        request =
            get url apiKey
    in
        Http.send ShowAccounts request


get : String -> String -> Http.Request (List Account)
get url token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson Decoders.accountsResponse
        , timeout = Nothing
        , withCredentials = False
        }
