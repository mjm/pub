module Micropub exposing
    ( Config
    , Session
    , getConfig
    , getPost
    , login
    , postTypeName
    , postTypes
    )

import Http
import IndieAuth as Auth
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Microformats
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


decodeConfig : D.Decoder Config
decodeConfig =
    D.succeed Config
        |> optional "media-endpoint" (D.maybe D.string) Nothing
        |> optional "post-types" (D.maybe (D.list decodePostType)) Nothing


decodePostType : D.Decoder PostType
decodePostType =
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


getConfig : Session -> (Result Http.Error Config -> msg) -> Cmd msg
getConfig session msg =
    Http.request
        { method = "GET"
        , headers = [ Auth.header session.token ]
        , url = session.url ++ "?q=config"
        , body = Http.emptyBody
        , expect = Http.expectJson msg decodeConfig
        , timeout = Nothing
        , tracker = Nothing
        }


getPost : String -> Session -> (Result Http.Error Microformats.Item -> msg) -> Cmd msg
getPost url session msg =
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
