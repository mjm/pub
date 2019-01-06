module Session exposing
    ( Data(..)
    , LoggedInData
    , decoder
    , empty
    , encode
    , login
    , updatePageData
    , updatePages
    )

import Blog.Page as Page
import IndieAuth as Auth
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
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
    , pages : List Page.Page
    }


empty : Data
empty =
    Guest


login : Micropub.Session -> Micropub.Config -> Data -> Data
login mp cfg sess =
    let
        newMp =
            { mp | mediaUrl = cfg.mediaEndpoint }
    in
    case sess of
        Guest ->
            Guest

        LoggingIn pd ->
            LoggedIn
                { micropub = newMp
                , config = cfg
                , pageData = pd
                , pages = []
                }

        LoggedIn data ->
            LoggedIn { data | micropub = newMp, config = cfg }


updatePageData : MPH.Data -> Data -> Data
updatePageData pageData sess =
    case sess of
        Guest ->
            Guest

        LoggingIn _ ->
            LoggingIn pageData

        LoggedIn data ->
            LoggedIn { data | pageData = pageData }


updatePages : List Page.Page -> Data -> Data
updatePages pages sess =
    case sess of
        LoggedIn data ->
            LoggedIn { data | pages = pages }

        _ ->
            sess


decoder : D.Decoder Data
decoder =
    D.oneOf
        [ D.map LoggedIn loggedInDecoder
        , D.map LoggingIn
            (D.field "pageData" MPH.localDecoder)
        , D.succeed Guest
        ]


loggedInDecoder : D.Decoder LoggedInData
loggedInDecoder =
    D.succeed LoggedInData
        |> required "micropub" Micropub.sessionDecoder
        |> required "config" Micropub.configDecoder
        |> required "pageData" MPH.localDecoder
        |> optional "pages" (D.list Page.decoder) []


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
        , ( "pages", E.list Page.encode data.pages )
        ]
