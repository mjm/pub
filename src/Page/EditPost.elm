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
import Html.Events exposing (onInput, onSubmit)
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
    | SetName String
    | SetContent String
    | SavePost


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPost (Ok post) ->
            ( { model | post = Just post }, Cmd.none )

        GotPost (Err _) ->
            ( model, Cmd.none )

        SetName name ->
            ( model, Cmd.none )

        SetContent name ->
            ( model, Cmd.none )

        SavePost ->
            ( model, Cmd.none )


view : Model -> Skeleton.Details Message
view model =
    { title = "It's Bloggin' Time!"
    , body =
        [ case model.post of
            Nothing ->
                p [] [ text "Loading post to edit..." ]

            Just post ->
                editPost post
        ]
    , session = model.session
    }


editPost : Microformats.Item -> Html Message
editPost item =
    Html.form
        [ class "w-full h-screen flex flex-col"
        , onSubmit SavePost
        ]
        [ case Microformats.string "url" item of
            Nothing ->
                text ""

            Just url ->
                div [] [ text ("Permalink: " ++ url) ]
        , div [ class "flex-initial py-2 border-orange border-b" ]
            [ input
                [ class "text-xl appearance-none w-full bg-transparent border-none focus:outline-none"
                , placeholder "Untitled"
                , onInput SetName
                , value (Maybe.withDefault "" (Microformats.string "name" item))
                ]
                []
            ]
        , div [ class "flex flex-col flex-grow mt-2" ]
            [ textarea
                [ class "w-full flex-grow focus:outline-none"
                , onInput SetContent
                , value (Maybe.withDefault "" (Microformats.string "content" item))
                ]
                []
            ]
        ]
