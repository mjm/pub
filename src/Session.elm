module Session exposing
    ( Data(..)
    , LoggedInData
    , empty
    , login
    , wrapLoggedIn
    )

import IndieAuth as Auth
import Micropub


type Data
    = Guest
    | LoggedIn LoggedInData


type alias LoggedInData =
    { micropub : Micropub.Session
    }


empty : Data
empty =
    Guest


login : String -> Auth.AuthorizedToken -> Data -> Data
login endpointUrl token _ =
    LoggedIn { micropub = Micropub.login endpointUrl token }


wrapLoggedIn : LoggedInData -> Data
wrapLoggedIn =
    LoggedIn
