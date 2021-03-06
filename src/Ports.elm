port module Ports exposing (clearSession, storePageData, storeSession)

import Json.Encode as E


port storePageData : E.Value -> Cmd msg


port storeSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg
