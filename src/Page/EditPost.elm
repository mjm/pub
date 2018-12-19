module Page.EditPost exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Editor
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
    , url : String
    , originalPost : Maybe Microformats.Item
    , post : Maybe Microformats.Item
    , diff : Maybe Diff.Diff
    , editor : Editor.State
    }


init : Session.LoggedInData -> String -> ( Model, Cmd Message )
init session url =
    ( { session = session
      , url = url
      , originalPost = Nothing
      , post = Nothing
      , diff = Nothing
      , editor = Editor.create
      }
    , MP.getPost GotPost url session.micropub
    )


type Message
    = NoOp
    | GotPost (Result Http.Error Microformats.Item)
    | SetName String
    | SetContent String
    | SetEditorState Editor.State
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

        SetEditorState editor ->
            ( { model | editor = editor }, Cmd.none )

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
    { title = "Edit Post"
    , body =
        [ case model.post of
            Nothing ->
                p [] [ text "Loading post to edit..." ]

            Just post ->
                editPost model post
        ]
    , session = model.session
    , selection = Skeleton.Post model.url
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
        [ div [ class "flex flex-none flex-row items-baseline" ]
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
                , class "font-semibold px-3 py-2 mx-1 rounded"
                , if hasChanges then
                    class "bg-grey-lighter text-grey-darker"

                  else
                    class "bg-grey-lightest text-grey"
                ]
                [ text "Revert" ]
            ]
        , div [ class "flex-none py-2 border-orange border-b" ]
            [ input
                [ class "px-2 text-xl appearance-none w-full bg-transparent border-none focus:outline-none"
                , placeholder "Untitled"
                , onInput SetName
                , value (Maybe.withDefault "" (Microformats.string "name" item))
                ]
                []
            ]
        , div [ class "flex flex-col flex-grow mt-3" ]
            [ Editor.view
                (Maybe.withDefault "" (Microformats.string "content" item))
                { onInput = SetContent
                , onStateChange = SetEditorState
                , attrs = [ class "w-full flex-grow" ]
                }
                model.editor
            ]
        ]
