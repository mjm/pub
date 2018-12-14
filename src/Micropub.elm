module Micropub exposing (Config, Endpoint, getConfig)

import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional)


type alias Endpoint =
    { url : String
    , token : String
    }


type alias Config =
    { mediaEndpoint : Maybe String
    }


decodeConfig : D.Decoder Config
decodeConfig =
    D.succeed Config
        |> optional "media-endpoint" (D.maybe D.string) Nothing


getConfig : Endpoint -> (Result Http.Error Config -> msg) -> Cmd msg
getConfig endpoint msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ endpoint.token) ]
        , url = endpoint.url ++ "?q=config"
        , body = Http.emptyBody
        , expect = Http.expectJson msg decodeConfig
        , timeout = Nothing
        , tracker = Nothing
        }
