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
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import IndieAuth as Auth
import Microformats
import Micropub as MP
import Micropub.Diff as Diff
import Micropub.Html as MPH
import Session
import Skeleton


type alias Model =
    { session : Session.LoggedInData
    , originalPost : Maybe Microformats.Item
    , post : Maybe Microformats.Item
    , diff : Maybe Diff.Diff
    }


init : Session.LoggedInData -> String -> ( Model, Cmd Message )
init session url =
    ( { session = session
      , originalPost = Nothing
      , post = Nothing
      , diff = Nothing
      }
    , MP.getPost GotPost url session.micropub
    )


type Message
    = NoOp
    | GotPost (Result Http.Error Microformats.Item)
    | SetName String
    | SetContent String
    | SavePost
    | SavedPost (Result Http.Error ())
    | RevertPost


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPost (Ok post) ->
            ( { model | originalPost = Just post, post = Just post }, Cmd.none )

        GotPost (Err _) ->
            ( model, Cmd.none )

        SetName name ->
            ( updatePost (Microformats.setString "name" name) model, Cmd.none )

        SetContent content ->
            ( updatePost (Microformats.setString "content" content) model, Cmd.none )

        SavePost ->
            case model.diff of
                Just d ->
                    ( model, MP.updatePost SavedPost d model.session.micropub )

                Nothing ->
                    ( model, Cmd.none )

        SavedPost (Ok _) ->
            ( { model | originalPost = model.post, diff = Nothing }, Cmd.none )

        SavedPost (Err _) ->
            ( model, Cmd.none )

        RevertPost ->
            ( { model | post = model.originalPost, diff = Nothing }, Cmd.none )


updatePost : (Microformats.Item -> Microformats.Item) -> Model -> Model
updatePost f model =
    let
        newPost =
            Maybe.map f model.post

        diff =
            Maybe.map2 Diff.diff model.originalPost newPost
    in
    { model | post = newPost, diff = diff }


view : Model -> Skeleton.Details Message
view model =
    { title = "It's Bloggin' Time!"
    , body =
        [ case model.post of
            Nothing ->
                p [] [ text "Loading post to edit..." ]

            Just post ->
                editPost model post
        ]
    , session = model.session
    }


editPost : Model -> Microformats.Item -> Html Message
editPost model item =
    let
        hasChanges =
            Maybe.withDefault False <| Maybe.map Diff.hasChanges model.diff
    in
    Html.form
        [ class "w-full h-screen flex flex-col"
        , onSubmit
            (if hasChanges then
                SavePost

             else
                NoOp
            )
        ]
        [ div [ class "flex flex-initial flex-row items-baseline" ]
            [ div [ class "flex-grow" ]
                [ case Microformats.string "url" item of
                    Nothing ->
                        text ""

                    Just url ->
                        span []
                            [ strong [ class "text-orange-darkest" ] [ text "Permalink: " ]
                            , a
                                [ href url
                                , class "text-orange-dark"
                                , target "_blank"
                                ]
                                [ text url ]
                            ]
                ]
            , button
                [ type_ "submit"
                , class "px-3 py-2 mx-1 rounded"
                , if hasChanges then
                    class "font-bold bg-blue-dark text-white"

                  else
                    class "bg-grey-lightest text-grey"
                ]
                [ text "Save" ]
            , button
                [ type_ "button"
                , onClick
                    (if hasChanges then
                        RevertPost

                     else
                        NoOp
                    )
                , class "font-medium px-3 py-2 mx-1 rounded"
                , if hasChanges then
                    class "bg-grey-lighter text-grey-darker"

                  else
                    class "bg-grey-lightest text-grey"
                ]
                [ text "Revert" ]
            ]
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
                [ class "w-full flex-grow focus:outline-none leading-normal"
                , onInput SetContent
                , value (Maybe.withDefault "" (Microformats.string "content" item))
                ]
                []
            ]
        ]
