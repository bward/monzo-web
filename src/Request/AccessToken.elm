module Request.AccessToken exposing (exchangeAuthorizationCode)

import Json.Decode exposing (Decoder, string, int, field, index, andThen, map, fail, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Http
import Data.AccessToken exposing (..)
import Data.Config exposing (..)


exchangeAuthorizationCode : Config -> String -> Http.Request AccessToken
exchangeAuthorizationCode config code =
    let
        body =
            Encode.object
                [ ( "grant_type", Encode.string "authorization_code" ) 
                , ( "client_id", Encode.string config.clientId)
                , ( "client_secret", Encode.string config.clientSecret)
                , ( "redirect_uri", Encode.string config.redirectUri )
                , ( "code", Encode.string code)
                ]

    in
        Http.post "https://api.monzo.com/oauth2/token" (Http.jsonBody body) accessToken


accessToken : Decoder AccessToken
accessToken =
    decode AccessToken
        |> required "access_token" string
        |> required "client_id" string
        |> required "expires_in" int
        |> required "token_type" string
        |> required "user_id" string
