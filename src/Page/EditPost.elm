module Page.EditPost exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Editor
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Events.Drag exposing (..)
import Http
import IndieAuth as Auth
import Microformats
import Micropub as MP
import Micropub.Diff as Diff
import Micropub.Html as MPH
import Session
import Skeleton
import Upload


type alias Model =
    { session : Session.LoggedInData
    , url : String
    , originalPost : Maybe Microformats.Item
    , post : Maybe Microformats.Item
    , diff : Maybe Diff.Diff
    , editor : Editor.State
    , draggingPhoto : Bool
    , photos : Upload.FileBag
    }


init : Session.LoggedInData -> String -> ( Model, Cmd Message )
init session url =
    ( { session = session
      , url = url
      , originalPost = Nothing
      , post = Nothing
      , diff = Nothing
      , editor = Editor.create
      , draggingPhoto = False
      , photos = Upload.emptyBag
      }
    , MP.getPost GotPost url session.micropub
    )


type Message
    = NoOp
    | GotPost (Result Http.Error Microformats.Item)
    | SetName String
    | SetContent String
    | SetPhoto (List File)
    | SetDraggingPhoto Bool
    | SetEditorState Editor.State
    | SavePost
    | SavedPost (Result Http.Error ())
    | RevertPost
    | GotPhotoUrl Upload.Tag String
    | UploadedPhoto Upload.Tag (Result Http.Error String)


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPost (Ok post) ->
            ( { model
                | originalPost = Just post
                , post = Just post
                , photos = existingFiles post
              }
            , Cmd.none
            )

        GotPost (Err _) ->
            ( model, Cmd.none )

        SetName name ->
            ( updatePost (Microformats.setString "name" name) model, Cmd.none )

        SetContent content ->
            ( updatePost (Microformats.setString "content" content) model, Cmd.none )

        SetPhoto files ->
            appendPhotos files model

        SetDraggingPhoto isDragging ->
            ( { model | draggingPhoto = isDragging }, Cmd.none )

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
            ( revertPost model, Cmd.none )

        GotPhotoUrl tag url ->
            ( { model | photos = Upload.setDataUrl tag url model.photos }, Cmd.none )

        UploadedPhoto i (Ok url) ->
            ( setUrl i url model, Cmd.none )

        UploadedPhoto _ (Err _) ->
            ( model, Cmd.none )


existingFiles : Microformats.Item -> Upload.FileBag
existingFiles post =
    case Microformats.strings "photo" post of
        Nothing ->
            Upload.emptyBag

        Just urls ->
            Upload.fromUrls urls


updatePost : (Microformats.Item -> Microformats.Item) -> Model -> Model
updatePost f model =
    let
        newPost =
            Maybe.map f model.post

        diff =
            Maybe.map2 Diff.diff model.originalPost newPost
    in
    { model | post = newPost, diff = diff }


appendPhotos : List File -> Model -> ( Model, Cmd Message )
appendPhotos files model =
    let
        ( newBag, cmds ) =
            Upload.append GotPhotoUrl UploadedPhoto model.session.micropub files model.photos
    in
    ( { model | photos = newBag, draggingPhoto = False }, cmds )


setUrl : Upload.Tag -> String -> Model -> Model
setUrl tag url model =
    let
        newFiles =
            Upload.setUploadedUrl tag url model.photos

        photos =
            Upload.urls newFiles

        updatedModel =
            updatePost (Microformats.setStrings "photo" photos) model
    in
    { updatedModel | photos = newFiles }


revertPost : Model -> Model
revertPost model =
    let
        photos =
            case model.originalPost of
                Just p ->
                    existingFiles p

                Nothing ->
                    Upload.emptyBag
    in
    { model | post = model.originalPost, diff = Nothing, photos = photos }


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

        photos =
            Upload.displayUrls model.photos
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
        , div
            [ class "mt-2 p-3 flex flex-col rounded-lg"
            , class
                (if model.draggingPhoto then
                    "bg-orange-lighter"

                 else
                    "bg-orange-lightest"
                )
            , class
                (if List.isEmpty photos then
                    "h-16"

                 else
                    "h-48"
                )
            , onDrop SetPhoto
            , onDragOver NoOp
            , onDragEnter (SetDraggingPhoto True)
            , onDragLeave (SetDraggingPhoto False)
            ]
            (if List.isEmpty photos then
                [ p [ class "my-auto text-center pointer-events-none text-sm uppercase font-semibold text-orange-dark" ]
                    [ text "Drop photos here" ]
                ]

             else
                [ h3 [ class "uppercase text-sm mb-2 text-orange-dark pointer-events-none" ]
                    [ text <| "Photos (" ++ String.fromInt (List.length photos) ++ ")" ]
                , p [ class "flex flex-row items-start overflow-hidden pointer-events-none" ] <|
                    List.map
                        (\u -> img [ src u, class "w-1/4 max-h-full pointer-events-auto" ] [])
                        photos
                ]
            )
        ]
