module Micropub exposing
    ( Session, login, encodeSession, sessionDecoder
    , Config, getConfig, encodeConfig, configDecoder, postTypes, getPostType
    , getPost, createPost, updatePost
    )

{-| This module provides support for interacting the Micropub API on sites that
support that specification.

This includes reading Micropub configuration, as well as getting, creating, and
updating posts.


# Sessions

@docs Session, login, encodeSession, sessionDecoder


# Configuration

@docs Config, getConfig, encodeConfig, configDecoder, postTypes, getPostType


# Posts

@docs getPost, createPost, updatePost

-}

import Dict
import Http
import Http.Util exposing (expectCreated)
import IndieAuth as Auth
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E
import Microformats
import Micropub.Diff as Diff
import Micropub.PostType as PostType exposing (PostType(..))
import Url.Builder as UB


{-| A session provides the ability to make requests to a particular Micropub
endpoint.

A session always has a URL and an authorized token, so unless one of those
values is invalid, it should always be possible to make Micropub requests if
you have a session.

A session may include a URL for a media endpoint if the Micropub endpoint has
one.

-}
type alias Session =
    { url : String
    , token : Auth.AuthorizedToken
    , mediaUrl : Maybe String
    }


{-| Create a Micropub session for a given endpoint and token.

With the created session, requests can be made to the Micropub endpoint.

Upon logging in, a session will not have a media endpoint even if the endpoint
supports it. This is because a session is needed to make requests, and a
request must be made for the configuration to know what the media endpoint is,
if any.

-}
login : String -> Auth.AuthorizedToken -> Session
login url token =
    { url = url, token = token, mediaUrl = Nothing }


{-| Represents the configuration information that can be obtained from the
Micropub endpoint.
-}
type alias Config =
    { mediaEndpoint : Maybe String
    , postTypes : Maybe (List PostType)
    }


{-| Returns a list of post types supported by the Micropub configuration.

If the configuration doesn't include a list of post types, this includes all
of the types we know of, with default names.

-}
postTypes : Config -> List PostType
postTypes cfg =
    Maybe.withDefault PostType.all cfg.postTypes


{-| Gets the PostType for type in the configuration that matches the given
canonical type name.

The configuration (or this library) may not include support for the post type,
in which case Nothing is returned.

-}
getPostType : Maybe String -> Config -> Maybe PostType
getPostType type_ cfg =
    let
        types =
            postTypes cfg
    in
    case type_ of
        Just n ->
            List.head <| List.filter (\pt -> PostType.key pt == n) types

        Nothing ->
            List.head types


{-| Decode a Micropub configuration from JSON.
-}
configDecoder : D.Decoder Config
configDecoder =
    D.succeed Config
        |> optional "media-endpoint" (D.maybe D.string) Nothing
        |> optional "post-types" (D.maybe (D.list PostType.decoder)) Nothing


{-| Encode a Micropub configuration to JSON.

This isn't used in communication with the Micropub API, but it can be used to
store configuration data locally to avoid having to ask the server for it every
time it is needed.

-}
encodeConfig : Config -> E.Value
encodeConfig cfg =
    E.object
        [ ( "media-endpoint", Maybe.withDefault E.null (Maybe.map E.string cfg.mediaEndpoint) )
        , ( "post-types"
          , Maybe.withDefault E.null
                (Maybe.map (E.list PostType.encode) cfg.postTypes)
          )
        ]


{-| Decode a Micropub session from JSON.
-}
sessionDecoder : D.Decoder Session
sessionDecoder =
    D.succeed Session
        |> required "url" D.string
        |> required "token" Auth.tokenDecoder
        |> optional "mediaUrl" (D.maybe D.string) Nothing


{-| Encode a Micropub session to JSON.

This can be used to store the session locally so that subsequent requests can
immediately begin making Micropub requests.

-}
encodeSession : Session -> E.Value
encodeSession sess =
    E.object
        [ ( "url", E.string sess.url )
        , ( "token", Auth.encodeToken sess.token )
        , ( "mediaUrl"
          , case sess.mediaUrl of
                Just u ->
                    E.string u

                Nothing ->
                    E.null
          )
        ]


{-| Fetches the configuration of a Micropub server.
-}
getConfig : (Result Http.Error Config -> msg) -> Session -> Cmd msg
getConfig msg session =
    Http.request
        { method = "GET"
        , headers = [ Auth.header session.token ]
        , url = session.url ++ "?q=config"
        , body = Http.emptyBody
        , expect = Http.expectJson msg configDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Gets the source version of a post by its URL.

The post will be in the form of a microformats2 item. This may differ from the
version of a post that can be parsed from the HTML page of the post, as it is
meant to be the representation the server uses for the post. For instance, the
content may be in Markdown instead of HTML.

This form of the post is suitable for editing, while a post that is parsed from
an HTML page may not be.

-}
getPost : (Result Http.Error Microformats.Item -> msg) -> String -> Session -> Cmd msg
getPost msg url session =
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
createPost : (Result Http.Error String -> msg) -> Microformats.Item -> Session -> Cmd msg
createPost msg item session =
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
updatePost : (Result Http.Error () -> msg) -> Diff.Diff -> Session -> Cmd msg
updatePost msg diff session =
    Http.request
        { method = "POST"
        , headers = [ Auth.header session.token ]
        , url = session.url
        , body = Http.jsonBody (Diff.encode diff)
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }
