module IndieAuth exposing (AuthorizedToken, authorizeToken, encodeToken, tokenDecoder)

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


authorizeToken : (Result Http.Error AuthorizedToken -> msg) -> String -> String -> String -> Cmd msg
authorizeToken msg code me state =
    Http.post
        { url = "https://blog-api.mattmoriarity.com/token"
        , body = Http.stringBody "application/x-www-form-urlencoded" (encodeAuthorizeBody code me)
        , expect = Http.expectJson msg tokenDecoder
        }


encodeAuthorizeBody : String -> String -> String
encodeAuthorizeBody code me =
    UB.toQuery
        [ UB.string "grant_type" "authorization_code"
        , UB.string "code" code
        , UB.string "client_id" "http://localhost:8000"
        , UB.string "redirect_uri" "http://localhost:8000/callback"
        , UB.string "me" me
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
