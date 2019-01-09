module Micropub.Post exposing (create, get, inferType, update)

import Http
import Http.Util exposing (expectCreated)
import IndieAuth as Auth
import Microformats
import Micropub as MP
import Micropub.Diff as Diff
import Micropub.PostType as PostType exposing (PostType(..))
import Url.Builder as UB


{-| Gets the source version of a post by its URL.

The post will be in the form of a microformats2 item. This may differ from the
version of a post that can be parsed from the HTML page of the post, as it is
meant to be the representation the server uses for the post. For instance, the
content may be in Markdown instead of HTML.

This form of the post is suitable for editing, while a post that is parsed from
an HTML page may not be.

-}
get : (Result Http.Error Microformats.Item -> msg) -> String -> MP.Session -> Cmd msg
get msg url session =
    let
        reqUrl =
            session.url
                ++ UB.toQuery
                    [ UB.string "q" "source"
                    , UB.string "url" url
                    ]
    in
    Http.request
        { method = "GET"
        , headers = [ Auth.header session.token ]
        , url = reqUrl
        , body = Http.emptyBody
        , expect = Http.expectJson msg Microformats.itemDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Creates a new post on the Micropub server.

If successful, the result in the message will contain the URL where the newly
created post can be found on the user's site. This URL can then be used to
display the post or edit it later.

-}
create : (Result Http.Error String -> msg) -> Microformats.Item -> MP.Session -> Cmd msg
create msg item session =
    Http.request
        { method = "POST"
        , headers = [ Auth.header session.token ]
        , url = session.url
        , body = Http.jsonBody (Microformats.encodeItem item)
        , expect = expectCreated msg
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Apply changes to an existing post.

In Micropub, changes to a post are made by describing specific changes to make
to individual properties. This is represented here as a Diff, which describes
which properties' values should be replaced, added to, deleted from.

-}
update : (Result Http.Error () -> msg) -> Diff.Diff -> MP.Session -> Cmd msg
update msg diff session =
    Http.request
        { method = "POST"
        , headers = [ Auth.header session.token ]
        , url = session.url
        , body = Http.jsonBody (Diff.encode diff)
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


inferType : MP.Config -> Microformats.Item -> PostType
inferType config item =
    possibleTypeKeys item
        |> List.filterMap (\x -> MP.getPostType (Just x) config)
        |> List.head
        |> Maybe.withDefault (Article "Article")


possibleTypeKeys : Microformats.Item -> List String
possibleTypeKeys item =
    let
        photos =
            Maybe.withDefault [] (Microformats.strings "photo" item)

        name =
            Maybe.withDefault "" (Microformats.string "name" item)

        checks =
            [ ( "photo", not (List.isEmpty photos) )
            , ( "note", String.isEmpty name )
            ]
    in
    checks
        |> List.filter Tuple.second
        |> List.map Tuple.first
