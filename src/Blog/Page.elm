module Blog.Page exposing
    ( Page
    , all
    , decoder
    , encode
    )

import Http
import IndieAuth as Auth
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Micropub as MP


type alias Page =
    { path : String
    , name : String
    , content : String
    }


decoder : D.Decoder Page
decoder =
    D.succeed Page
        |> required "path" D.string
        |> required "name" D.string
        |> required "content" D.string


encode : Page -> E.Value
encode page =
    E.object
        [ ( "path", E.string page.path )
        , ( "name", E.string page.name )
        , ( "content", E.string page.content )
        ]


pagesUrl : MP.Session -> String
pagesUrl session =
    String.replace "/micropub" "/pages" session.url


all : (Result Http.Error (List Page) -> msg) -> MP.Session -> Cmd msg
all toMsg session =
    Http.request
        { method = "GET"
        , headers = [ Auth.header session.token ]
        , url = pagesUrl session
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (D.list decoder)
        , timeout = Nothing
        , tracker = Nothing
        }
