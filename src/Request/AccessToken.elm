module Request.AccessToken exposing (exchangeAuthorizationCode)

import Json.Decode exposing (Decoder, string, int, field, index, andThen, map, fail, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Http
import Data.AccessToken exposing (..)
import Data.Config exposing (..)


exchangeAuthorizationCode : Config -> String -> Http.Request AccessToken
exchangeAuthorizationCode config code =
    let
        body =
            Http.multipartBody
                [ (Http.stringPart "grant_type" "authorization_code")
                , (Http.stringPart "client_id" config.clientId)
                , (Http.stringPart "client_secret" config.clientSecret)
                , (Http.stringPart "redirect_uri" config.redirectUri)
                , (Http.stringPart "code" code)
                ]
    in
        Http.post "https://api.monzo.com/oauth2/token" body accessToken


accessToken : Decoder AccessToken
accessToken =
    decode AccessToken
        |> required "access_token" string
        |> required "client_id" string
        |> required "expires_in" int
        |> required "token_type" string
        |> required "user_id" string
