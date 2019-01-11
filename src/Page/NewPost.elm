module Page.NewPost exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
import Editor
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import IndieAuth as Auth
import Microformats
import Micropub as MP
import Micropub.Html as MPH
import Micropub.Post as Post
import Micropub.PostType as PostType exposing (PostType(..))
import Session
import Skeleton
import Urls
import View.Alert as Alert exposing (Alert, Alerts)
import View.Button as Button
import View.Photos as Photos
import View.PostForm as PostForm


type alias Model =
    { key : Nav.Key
    , session : Session.LoggedInData
    , alerts : Alerts Message
    , post : Microformats.Item
    , postType : PostType
    , isSaving : Bool
    , editor : Editor.State
    , photos : Photos.Model
    }


init : Nav.Key -> Session.LoggedInData -> PostType -> ( Model, Cmd Message )
init key session postType =
    ( { key = key
      , alerts = Alert.empty DismissAlert
      , session = session
      , post = Microformats.createEntry
      , postType = postType
      , isSaving = False
      , editor = Editor.create
      , photos = Photos.init session.micropub
      }
    , Cmd.none
    )


type Message
    = NoOp
    | DismissAlert Alert
    | SetName String
    | SetContent String
    | SetEditorState Editor.State
    | SavePost
    | SavedPost (Result Http.Error String)
    | PhotosMsg Photos.Message


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DismissAlert alert ->
            ( { model | alerts = Alert.dismiss alert model.alerts }, Cmd.none )

        SetName name ->
            ( updatePost (Microformats.setString "name" name) model, Cmd.none )

        SetContent content ->
            ( updatePost (Microformats.setString "content" content) model, Cmd.none )

        SetEditorState editor ->
            ( { model | editor = editor }, Cmd.none )

        SavePost ->
            ( { model | isSaving = True }
            , Post.create SavedPost model.post model.session.micropub
            )

        SavedPost (Ok url) ->
            ( { model | isSaving = False }
            , Nav.pushUrl model.key (Urls.editPost url)
            )

        SavedPost (Err err) ->
            ( { model
                | isSaving = False
                , alerts = Alert.append (Alert.fromHttpError err) model.alerts
              }
            , Cmd.none
            )

        PhotosMsg m ->
            updatePhotos m model


updatePost : (Microformats.Item -> Microformats.Item) -> Model -> Model
updatePost f model =
    { model | post = f model.post }


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


view : Model -> Skeleton.Details Message
view model =
    { title = "New " ++ PostType.name model.postType
    , body = [ editPost model ]
    , session = model.session
    , selection = Skeleton.Empty
    , alerts = model.alerts
    }


editPost : Model -> Html Message
editPost model =
    let
        isValid =
            True

        saveState =
            if model.isSaving then
                Button.Working

            else if isValid then
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
                [ h3 [] [ text ("New " ++ PostType.name model.postType) ]
                ]
            , Button.save saveState
            ]
        , PostForm.nameField SetName model.postType model.post
        , PostForm.contentField
            { onInput = SetContent
            , onStateChange = SetEditorState
            }
            model.postType
            model.post
            model.editor
        , PostForm.photoField PhotosMsg model.postType model.photos
        ]
