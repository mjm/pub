module IndieAuth exposing
    ( AuthorizationRequest, Callback, AuthorizedToken
    , begin, authorizeToken
    , header
    , encodeToken, tokenDecoder
    )

{-| This module supports using IndieAuth to allow a user to login with their
website URL.

It includes types and functions for making and handling the requests needed to
do the OAuth authorization flow for IndieAuth.

This logic is not specific to any particular authorization or token endpoints.
Instead, it's expected that the endpoints to use will be discovered by parsing
them from the user's website when they attempt to login.


# Definitions

@docs AuthorizationRequest, Callback, AuthorizedToken


# The Authorization Workflow

@docs begin, authorizeToken


# Using a Token

@docs header


# Serialization

@docs encodeToken, tokenDecoder

-}

import Browser.Navigation as Nav
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Url
import Url.Builder as UB


{-| Represents a token that was obtained from performing the IndieAuth flow
successfully.

  - `accessToken` to be included as a `Bearer` token in subsequence HTTP
    requests
  - `me`, the URL of the user's site. This is equivalent to a username.
  - `scopes` that the token is authorized for. This is mainly
    informational, as the server will check the token and determine what
    actions are allowed.

-}
type alias AuthorizedToken =
    { accessToken : String
    , me : String
    , scopes : List String
    }


{-| Information used to begin a new authorization flow.

  - `clientId`, the URL of the app requesting the authorization
  - `redirectUri` where the authorization endpoint should send the user once
    they have successfully been authorized. This must be the same origin as
    the URL of the `clientId`.
  - `me`, the URL of the user's site.
  - `scopes` that the app is requesting for their token to be able to perform
    actions on the user's behalf.

-}
type alias AuthorizationRequest =
    { clientId : String
    , redirectUri : String
    , me : String
    , scopes : List String
    }


{-| Information that is passed back to the app after authorization is
successful with the authorization endpoint.

  - `code` that can be exchanged with a token endpoint for a token.
  - `me`, the URL of the user's site who started the authorization flow.
  - `state` value that was passed to the authorization endpoint by the app
    when starting the authorization flow.

-}
type alias Callback =
    { code : String
    , me : String
    , state : String
    }


{-| Create a valid `Authorization` header using a token.
-}
header : AuthorizedToken -> Http.Header
header token =
    Http.header "Authorization" ("Bearer " ++ token.accessToken)


{-| Start an authorization flow with the given authorization endpoint.

This command will cause the user to leave this app and load a page from the
authorization endpoint.

-}
begin : AuthorizationRequest -> String -> Cmd msg
begin request endpoint =
    let
        authUrl =
            endpoint
                ++ UB.toQuery
                    [ UB.string "me" request.me
                    , UB.string "client_id" request.clientId
                    , UB.string "redirect_uri" request.redirectUri
                    , UB.string "state" "foo"
                    , UB.string "response_type" "code"
                    , UB.string "scope" (String.join " " request.scopes)
                    ]
    in
    Nav.load authUrl


{-| Exchange an authorization code for a token that can be used as
authorization credentials for the user.
-}
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


{-| Decode a response from a token endpoint.

This can also be used to decode a token that was serialized to JSON (for local
storage) using `encodeToken`.

-}
tokenDecoder : D.Decoder AuthorizedToken
tokenDecoder =
    D.succeed AuthorizedToken
        |> required "access_token" D.string
        |> required "me" D.string
        |> required "scope" scopesDecoder


scopesDecoder : D.Decoder (List String)
scopesDecoder =
    D.map (String.split " ") D.string


{-| Encode an authorized token to JSON.

This is not needed for the authorization workflow, but it may be useful for
storing a token locally once the workflow is complete in order to avoid having
to login on every page load.

-}
encodeToken : AuthorizedToken -> E.Value
encodeToken token =
    E.object
        [ ( "access_token", E.string token.accessToken )
        , ( "me", E.string token.me )
        , ( "scope", E.string (String.join " " token.scopes) )
        ]
