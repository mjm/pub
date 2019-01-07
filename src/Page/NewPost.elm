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
import Micropub.PostType as PostType exposing (PostType(..))
import Session
import Skeleton
import Urls
import View.Button as Button
import View.Photos as Photos


type alias Model =
    { key : Nav.Key
    , session : Session.LoggedInData
    , post : Microformats.Item
    , postType : PostType
    , isSaving : Bool
    , editor : Editor.State
    , photos : Photos.Model
    }


init : Nav.Key -> Session.LoggedInData -> PostType -> ( Model, Cmd Message )
init key session postType =
    ( { key = key
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

        SetName name ->
            ( updatePost (Microformats.setString "name" name) model, Cmd.none )

        SetContent content ->
            ( updatePost (Microformats.setString "content" content) model, Cmd.none )

        SetEditorState editor ->
            ( { model | editor = editor }, Cmd.none )

        SavePost ->
            ( { model | isSaving = True }
            , MP.createPost SavedPost model.post model.session.micropub
            )

        SavedPost (Ok url) ->
            ( { model | isSaving = False }
            , Nav.pushUrl model.key (Urls.editPost url)
            )

        SavedPost (Err _) ->
            ( { model | isSaving = False }, Cmd.none )

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


type Field
    = Name
    | Content
    | Photo


supportedFields : PostType -> List Field
supportedFields t =
    case t of
        Note _ ->
            [ Content ]

        Article _ ->
            [ Name, Content ]

        PostType.Photo _ ->
            [ Content, Photo ]

        Unknown _ _ ->
            []


view : Model -> Skeleton.Details Message
view model =
    { title = "New " ++ PostType.name model.postType
    , body = [ editPost model ]
    , session = model.session
    , selection = Skeleton.Empty
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

        fields =
            supportedFields model.postType

        displayField f =
            List.member f fields
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
        , if displayField Name then
            div [ class "flex-none py-2 border-orange border-b" ]
                [ input
                    [ class "px-2 text-xl appearance-none w-full bg-transparent border-none focus:outline-none"
                    , placeholder "Untitled"
                    , onInput SetName
                    , value (Maybe.withDefault "" (Microformats.string "name" model.post))
                    ]
                    []
                ]

          else
            text ""
        , if displayField Content then
            div [ class "flex flex-col flex-grow mt-3" ]
                [ Editor.view
                    (Maybe.withDefault "" (Microformats.string "content" model.post))
                    { onInput = SetContent
                    , onStateChange = SetEditorState
                    , attrs = [ class "w-full flex-grow" ]
                    }
                    model.editor
                ]

          else
            text ""
        , if displayField Photo then
            Photos.view
                { attrs = []
                , toMsg = PhotosMsg
                }
                model.photos

          else
            text ""
        ]
