module Micropub.Html exposing (Data, load)

import Dict
import Http
import Json.Decode as D
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
