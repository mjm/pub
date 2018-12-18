module Micropub exposing
    ( Config
    , PostType(..)
    , Session
    , configDecoder
    , createPost
    , encodeConfig
    , encodeSession
    , getConfig
    , getPost
    , getPostType
    , login
    , postTypeKey
    , postTypeName
    , postTypes
    , sessionDecoder
    , updatePost
    )

import Dict
import Http
import IndieAuth as Auth
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E
import Microformats
import Micropub.Diff as Diff
import Url.Builder as UB


type alias Session =
    { url : String
    , token : Auth.AuthorizedToken
    }


login : String -> Auth.AuthorizedToken -> Session
login =
    Session


type alias Config =
    { mediaEndpoint : Maybe String
    , postTypes : Maybe (List PostType)
    }


type PostType
    = Note String
    | Article String
    | Photo String
    | Unknown String String


allPostTypes : List PostType
allPostTypes =
    [ Note "Note"
    , Article "Article"
    , Photo "Photo"
    ]


postTypes : Config -> List PostType
postTypes cfg =
    Maybe.withDefault allPostTypes cfg.postTypes


postTypeName : PostType -> String
postTypeName t =
    case t of
        Note n ->
            n

        Article n ->
            n

        Photo n ->
            n

        Unknown n _ ->
            n


postTypeKey : PostType -> String
postTypeKey t =
    case t of
        Note _ ->
            "note"

        Article _ ->
            "article"

        Photo _ ->
            "photo"

        Unknown _ type_ ->
            type_


getPostType : Maybe String -> Config -> Maybe PostType
getPostType type_ cfg =
    let
        types =
            postTypes cfg
    in
    case type_ of
        Just n ->
            List.head <| List.filter (\pt -> postTypeKey pt == n) types

        Nothing ->
            List.head types


configDecoder : D.Decoder Config
configDecoder =
    D.succeed Config
        |> optional "media-endpoint" (D.maybe D.string) Nothing
        |> optional "post-types" (D.maybe (D.list postTypeDecoder)) Nothing


postTypeDecoder : D.Decoder PostType
postTypeDecoder =
    D.succeed
        (\name type_ ->
            case type_ of
                "note" ->
                    Note name

                "article" ->
                    Article name

                "photo" ->
                    Photo name

                _ ->
                    Unknown name type_
        )
        |> required "name" D.string
        |> required "type" D.string


encodeConfig : Config -> E.Value
encodeConfig cfg =
    E.object
        [ ( "media-endpoint", Maybe.withDefault E.null (Maybe.map E.string cfg.mediaEndpoint) )
        , ( "post-types", Maybe.withDefault E.null (Maybe.map encodePostTypes cfg.postTypes) )
        ]


encodePostTypes : List PostType -> E.Value
encodePostTypes =
    E.list
        (\t ->
            let
                ( name, type_ ) =
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
                [ ( "name", E.string name )
                , ( "type", E.string type_ )
                ]
        )


sessionDecoder : D.Decoder Session
sessionDecoder =
    D.succeed Session
        |> required "url" D.string
        |> required "token" Auth.tokenDecoder


encodeSession : Session -> E.Value
encodeSession sess =
    E.object
        [ ( "url", E.string sess.url )
        , ( "token", Auth.encodeToken sess.token )
        ]


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


expectCreated : (Result Http.Error String -> msg) -> Http.Expect msg
expectCreated toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case metadata.statusCode of
                        201 ->
                            case Dict.get "location" metadata.headers of
                                Just url ->
                                    Ok url

                                Nothing ->
                                    Err (Http.BadBody "No Location header found")

                        _ ->
                            Err (Http.BadStatus metadata.statusCode)
