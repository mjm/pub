module Session exposing
    ( Data(..)
    , LoggedInData
    , decoder
    , empty
    , encode
    , login
    )

import IndieAuth as Auth
import Json.Decode as D
import Json.Encode as E
import Microformats
import Micropub
import Micropub.Html as MPH


type Data
    = Guest
    | LoggingIn MPH.Data
    | LoggedIn LoggedInData


type alias LoggedInData =
    { micropub : Micropub.Session
    , config : Micropub.Config
    , pageData : MPH.Data
    }


empty : Data
empty =
    Guest


login : Micropub.Session -> Micropub.Config -> Data -> Data
login mp cfg sess =
    case sess of
        Guest ->
            Guest

        LoggingIn pd ->
            LoggedIn
                { micropub = mp
                , config = cfg
                , pageData = pd
                }

        LoggedIn data ->
            LoggedIn { data | micropub = mp, config = cfg }


decoder : D.Decoder Data
decoder =
    D.oneOf
        [ D.map3 (\x y z -> LoggedIn (LoggedInData x y z))
            (D.field "micropub" Micropub.sessionDecoder)
            (D.field "config" Micropub.configDecoder)
            (D.field "pageData" MPH.localDecoder)
        , D.map LoggingIn
            (D.field "pageData" MPH.localDecoder)
        , D.succeed Guest
        ]


encode : Data -> E.Value
encode data =
    case data of
        Guest ->
            E.object []

        LoggingIn pd ->
            E.object [ ( "pageData", MPH.encodeLocal pd ) ]

        LoggedIn loggedIn ->
            encodeLoggedIn loggedIn


encodeLoggedIn : LoggedInData -> E.Value
encodeLoggedIn data =
    E.object
        [ ( "pageData", MPH.encodeLocal data.pageData )
        , ( "config", Micropub.encodeConfig data.config )
        , ( "micropub", Micropub.encodeSession data.micropub )
        ]
