module Micropub.PostType exposing (PostType(..), all, decoder, encode, key, name)

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


{-| Represents a type of post that is supported by a Micropub endpoint.

There is an extension to Micropub that includes this information in the config
response. It can be used by clients to only allow creating post types that the
server claims to support.

All values include a string for a name, as the server can choose to assign its
own name to a well-known post type. A client should use this name in UI. For
instance, the "note" type could be named "Status Update" or "Tweet."

-}
type PostType
    = Note String
    | Article String
    | Photo String
    | Unknown String String


all : List PostType
all =
    [ Note "Note"
    , Article "Article"
    , Photo "Photo"
    ]


{-| Returns the displayable name of a post type.
-}
name : PostType -> String
name t =
    case t of
        Note n ->
            n

        Article n ->
            n

        Photo n ->
            n

        Unknown n _ ->
            n


{-| Returns the canonical type name of the post type.

This should be a name that is [documented in the IndieWeb wiki][types], such
that it has shared meaning across various clients and servers.

[types]: https://indieweb.org/Category:PostType

-}
key : PostType -> String
key t =
    case t of
        Note _ ->
            "note"

        Article _ ->
            "article"

        Photo _ ->
            "photo"

        Unknown _ type_ ->
            type_


decoder : D.Decoder PostType
decoder =
    D.succeed
        (\n type_ ->
            case type_ of
                "note" ->
                    Note n

                "article" ->
                    Article n

                "photo" ->
                    Photo n

                _ ->
                    Unknown n type_
        )
        |> required "name" D.string
        |> required "type" D.string


encode : PostType -> E.Value
encode t =
    let
        ( name_, type_ ) =
            case t of
                Note n ->
                    ( n, "note" )

                Article n ->
                    ( n, "article" )

                Photo n ->
                    ( n, "photo" )

                Unknown n x ->
                    ( n, x )
    in
    E.object
        [ ( "name", E.string name_ )
        , ( "type", E.string type_ )
        ]
