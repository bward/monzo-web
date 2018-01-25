module Types exposing (..)

import Accounts.Types


type alias Model =
    { accountsModel : Accounts.Types.Model }


type Msg
    = AccountsMsg Accounts.Types.Msg
