module Page.Home exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Html exposing (..)
import IndieAuth as Auth
import Session


type alias Model =
    { session : Session.Data
    , token : Auth.AuthorizedToken
    }


init : Session.Data -> Auth.AuthorizedToken -> ( Model, Cmd Message )
init session token =
    ( { session = session
      , token = token
      }
    , Cmd.none
    )


type Message
    = NoOp


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    ( model, Cmd.none )


view : Model -> Browser.Document Message
view model =
    { title = "It's Bloggin' Time!"
    , body =
        [ h1 [] [ text "You're logged in!" ]
        , p [] [ text <| "You signed in with your site " ++ model.token.me ]
        , p [] [ text <| "Your auth token is " ++ model.token.accessToken ]
        ]
    }
