module Micropub.Html exposing (Data, encodeLocal, load, localDecoder)

import Dict
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Microformats


type alias Data =
    { micropubEndpoint : Maybe String
    , authorizationEndpoint : Maybe String
    , tokenEndpoint : Maybe String
    , entries : List Microformats.Item
    }


load : (Result Http.Error Data -> msg) -> String -> Cmd msg
load msg url =
    Http.request
        { method = "POST"
        , headers = []
        , url = "https://blog-api.mattmoriarity.com/discover"
        , body = Http.jsonBody (E.object [ ( "url", E.string url ) ])
        , expect = Http.expectJson msg dataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


dataDecoder : D.Decoder Data
dataDecoder =
    D.map convertToData Microformats.decoder


convertToData : Microformats.Parsed -> Data
convertToData parsed =
    { micropubEndpoint = Microformats.getLink "micropub" parsed
    , authorizationEndpoint = Microformats.getLink "authorization_endpoint" parsed
    , tokenEndpoint = Microformats.getLink "token_endpoint" parsed
    , entries = Microformats.feedEntries parsed
    }


localDecoder : D.Decoder Data
localDecoder =
    D.succeed Data
        |> required "micropub" (D.nullable D.string)
        |> required "authorization_endpoint" (D.nullable D.string)
        |> required "token_endpoint" (D.nullable D.string)
        |> required "entries" (D.list Microformats.itemDecoder)


encodeLocal : Data -> E.Value
encodeLocal data =
    E.object
        [ ( "micropub", encodeMaybeString data.micropubEndpoint )
        , ( "authorization_endpoint", encodeMaybeString data.authorizationEndpoint )
        , ( "token_endpoint", encodeMaybeString data.tokenEndpoint )
        , ( "entries", E.list Microformats.encodeItem data.entries )
        ]


encodeMaybeString : Maybe String -> E.Value
encodeMaybeString str =
    Maybe.withDefault E.null (Maybe.map E.string str)
