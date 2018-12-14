module Session exposing
    ( Data
    , addToken
    , empty
    )

import IndieAuth as Auth
import Micropub


type alias Data =
    { endpoint : Maybe Micropub.Endpoint
    , token : Maybe Auth.AuthorizedToken
    }


empty : Data
empty =
    { endpoint = Nothing
    , token = Nothing
    }


addToken : Auth.AuthorizedToken -> Data -> Data
addToken token data =
    { data | token = Just token }
