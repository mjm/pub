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
import Micropub.Post as Post
import Micropub.PostType as PostType exposing (PostType)
import Session
import Skeleton
import View.Button as Button
import View.Photos as Photos
import View.PostForm as PostForm


type alias Model =
    { session : Session.LoggedInData
    , url : String
    , originalPost : Maybe Microformats.Item
    , post : Maybe Microformats.Item
    , postType : Maybe PostType
    , diff : Maybe Diff.Diff
    , isSaving : Bool
    , editor : Editor.State
    , photos : Photos.Model
    }


init : Session.LoggedInData -> String -> ( Model, Cmd Message )
init session url =
    ( { session = session
      , url = url
      , originalPost = Nothing
      , post = Nothing
      , postType = Nothing
      , diff = Nothing
      , isSaving = False
      , editor = Editor.create
      , photos = Photos.init session.micropub
      }
    , Post.get GotPost url session.micropub
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
    | PhotosMsg Photos.Message


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPost (Ok post) ->
            ( { model
                | originalPost = Just post
                , post = Just post
                , postType = Just (Post.inferType model.session.config post)
                , photos = Photos.setUrls (existingPhotos post) model.photos
              }
            , Cmd.none
            )

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
                    ( { model | isSaving = True }
                    , Post.update SavedPost d model.session.micropub
                    )

                Nothing ->
                    ( model, Cmd.none )

        SavedPost (Ok _) ->
            ( { model
                | originalPost = model.post
                , diff = Nothing
                , isSaving = False
              }
            , Cmd.none
            )

        SavedPost (Err _) ->
            ( { model | isSaving = False }, Cmd.none )

        RevertPost ->
            ( revertPost model, Cmd.none )

        PhotosMsg m ->
            updatePhotos m model


existingPhotos : Microformats.Item -> List String
existingPhotos post =
    Maybe.withDefault [] (Microformats.strings "photo" post)


updatePost : (Microformats.Item -> Microformats.Item) -> Model -> Model
updatePost f model =
    let
        newPost =
            Maybe.map f model.post

        diff =
            Maybe.map2 Diff.diff model.originalPost newPost
    in
    { model | post = newPost, diff = diff }


updatePhotos : Photos.Message -> Model -> ( Model, Cmd Message )
updatePhotos msg model =
    let
        ( newPhotos, cmds ) =
            Photos.update msg model.photos

        newModel =
            updatePost (Microformats.setStrings "photo" (Photos.urls newPhotos)) model
    in
    ( { newModel | photos = newPhotos }
    , Cmd.map PhotosMsg cmds
    )


revertPost : Model -> Model
revertPost model =
    let
        urls =
            case model.originalPost of
                Just p ->
                    existingPhotos p

                Nothing ->
                    []
    in
    { model
        | post = model.originalPost
        , diff = Nothing
        , photos = Photos.setUrls urls model.photos
    }


view : Model -> Skeleton.Details Message
view model =
    { title = "Edit Post"
    , body =
        [ case ( model.post, model.postType ) of
            ( Just post, Just t ) ->
                editPost model post t

            _ ->
                p [] [ text "Loading post to edit..." ]
        ]
    , session = model.session
    , selection = Skeleton.Post model.url
    }


editPost : Model -> Microformats.Item -> PostType -> Html Message
editPost model item t =
    let
        hasChanges =
            Maybe.withDefault False <| Maybe.map Diff.hasChanges model.diff

        saveState =
            if model.isSaving then
                Button.Working

            else if hasChanges then
                Button.Enabled

            else
                Button.Disabled
    in
    Html.form
        [ class "w-full h-screen flex flex-col"
        , onSubmit
            (if saveState == Button.Enabled then
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
                            [ strong [ class "text-orange-darker" ]
                                [ i [ class "fas fa-link mr-1" ] [] ]
                            , a
                                [ href url
                                , class "text-orange-dark no-underline"
                                , target "_blank"
                                ]
                                [ text url ]
                            ]
                ]
            , Button.save saveState
            , Button.revert RevertPost hasChanges
            ]
        , PostForm.nameField SetName t item
        , PostForm.contentField
            { onInput = SetContent
            , onStateChange = SetEditorState
            }
            t
            item
            model.editor
        , PostForm.photoField PhotosMsg t model.photos
        ]
