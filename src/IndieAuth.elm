module IndieAuth exposing (AuthorizedToken, Callback, authorizeToken, encodeToken, header, tokenDecoder)

import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Url
import Url.Builder as UB


type alias AuthorizedToken =
    { accessToken : String
    , me : String
    , scopes : List String
    }


type alias Callback =
    { code : String
    , me : String
    , state : String
    }


header : AuthorizedToken -> Http.Header
header token =
    Http.header "Authorization" ("Bearer " ++ token.accessToken)


authorizeToken : (Result Http.Error AuthorizedToken -> msg) -> String -> String -> Callback -> Cmd msg
authorizeToken msg url client cb =
    Http.post
        { url = url
        , body = Http.stringBody "application/x-www-form-urlencoded" (encodeAuthorizeBody client cb)
        , expect = Http.expectJson msg tokenDecoder
        }


encodeAuthorizeBody : String -> Callback -> String
encodeAuthorizeBody client cb =
    UB.toQuery
        [ UB.string "grant_type" "authorization_code"
        , UB.string "code" cb.code
        , UB.string "client_id" client
        , UB.string "redirect_uri" (client ++ "callback")
        , UB.string "me" cb.me
        ]
        |> String.dropLeft 1


tokenDecoder : D.Decoder AuthorizedToken
tokenDecoder =
    D.succeed AuthorizedToken
        |> required "access_token" D.string
        |> required "me" D.string
        |> required "scope" scopesDecoder


scopesDecoder : D.Decoder (List String)
scopesDecoder =
    D.map (String.split " ") D.string


encodeToken : AuthorizedToken -> E.Value
encodeToken token =
    E.object
        [ ( "access_token", E.string token.accessToken )
        , ( "me", E.string token.me )
        , ( "scope", E.string (String.join " " token.scopes) )
        ]
