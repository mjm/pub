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
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
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
    { title = "Login to your Blog"
    , body =
        [ div [ class "w-1/3 bg-white mx-auto mt-16 p-6 shadow-md rounded-lg" ]
            [ header []
                [ h1 [ class "text-orange-darkest font-thin text-center" ] [ text "Log in to your blog" ] ]
            , p [ class "text-center text-grey-darker text-sm my-4" ]
                [ text "Enter your domain to log in using IndieAuth." ]
            , Html.form [ onSubmit Login ]
                [ div [ class "flex items-center py-2 border-orange border-b" ]
                    [ input
                        [ class "appearance-none w-full bg-transparent border-none focus:outline-none"
                        , type_ "text"
                        , placeholder "https://example.com"
                        , value model.siteUrl
                        , autofocus True
                        , onInput SetSiteUrl
                        ]
                        []
                    , button
                        [ type_ "submit"
                        , class "flex-no-shrink bg-orange hover:bg-orange-dark border-4 border-orange hover:border-orange-dark text-white text-md py-1 px-2 rounded"
                        ]
                        [ text "Login" ]
                    ]
                ]
            ]
        ]
    }
