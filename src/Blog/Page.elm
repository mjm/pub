module Blog.Page exposing
    ( Page
    , all
    , decoder
    , empty
    , encode
    , get
    , hasChanges
    , setShortPath
    , shortPath
    , update
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


empty : Page
empty =
    { path = "pages/"
    , name = ""
    , content = ""
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


shortPath : Page -> String
shortPath page =
    String.replace "pages/" "" page.path


setShortPath : String -> Page -> Page
setShortPath path page =
    { page | path = "pages/" ++ path }


hasChanges : Page -> Page -> Bool
hasChanges old new =
    (old.name /= new.name) || (old.content /= new.content)


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


pageUrl : MP.Session -> String -> String
pageUrl session path =
    pagesUrl session ++ "/" ++ path


get : (Result Http.Error Page -> msg) -> String -> MP.Session -> Cmd msg
get toMsg path session =
    Http.request
        { method = "GET"
        , headers = [ Auth.header session.token ]
        , url = pageUrl session path
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


update : (Result Http.Error () -> msg) -> Page -> MP.Session -> Cmd msg
update toMsg page session =
    Http.request
        { method = "PUT"
        , headers = [ Auth.header session.token ]
        , url = pageUrl session (shortPath page)
        , body = Http.jsonBody (encode page)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }
