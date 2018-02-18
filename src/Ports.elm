port module Ports exposing (..)

import Data.AccessToken exposing (AccessToken)

port saveAccessToken : AccessToken -> Cmd msg