module Page.Login exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Session
import Url.Builder as UB


type alias Model =
    { session : Session.Data
    , siteUrl : String
    }


init : Session.Data -> ( Model, Cmd Message )
init session =
    ( { session = session
      , siteUrl = ""
      }
    , Cmd.none
    )


type Message
    = NoOp
    | SetSiteUrl String
    | Login


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSiteUrl url ->
            ( { model | siteUrl = url }, Cmd.none )

        Login ->
            ( model, loadAuthPage model.siteUrl )


loadAuthPage : String -> Cmd msg
loadAuthPage url =
    let
        authUrl =
            UB.crossOrigin "https://indieauth.com"
                [ "auth" ]
                [ UB.string "me" url
                , UB.string "client_id" "http://localhost:8000"
                , UB.string "redirect_uri" "http://localhost:8000/callback"
                , UB.string "state" "foo"
                , UB.string "response_type" "code"
                , UB.string "scope" "create update delete"
                ]
    in
    Nav.load authUrl


view : Model -> Browser.Document Message
view model =
    { title = "Login"
    , body =
        [ header []
            [ h1 [] [ text "Login to Blog Client" ] ]
        , p [] [ text "You need to login to access this tool" ]
        , section []
            [ input
                [ type_ "text"
                , placeholder "https://example.com"
                , value model.siteUrl
                , onInput SetSiteUrl
                ]
                []
            , button [ onClick Login ] [ text "Login" ]
            ]
        ]
    }
