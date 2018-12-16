module Page.EditPost exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import IndieAuth as Auth
import Microformats
import Micropub as MP
import Micropub.Html as MPH
import Session
import Skeleton


type alias Model =
    { session : Session.LoggedInData
    , post : Maybe Microformats.Item
    }


init : Session.LoggedInData -> String -> ( Model, Cmd Message )
init session url =
    ( { session = session
      , post = Nothing
      }
    , MP.getPost GotPost url session.micropub
    )


type Message
    = NoOp
    | GotPost (Result Http.Error Microformats.Item)


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPost (Ok post) ->
            ( { model | post = Just post }, Cmd.none )

        GotPost (Err _) ->
            ( model, Cmd.none )


view : Model -> Skeleton.Details Message
view model =
    { title = "It's Bloggin' Time!"
    , body =
        [ case model.post of
            Nothing ->
                p [] [ text "Loading post to edit..." ]

            Just post ->
                p [] [ text <| Maybe.withDefault "" (Microformats.string "content" post) ]
        ]
    , session = model.session
    }
