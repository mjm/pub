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
import Session
import Skeleton
import Urls


type alias Model =
    { key : Nav.Key
    , session : Session.LoggedInData
    , post : Microformats.Item
    , postType : MP.PostType
    , editor : Editor.State
    }


init : Nav.Key -> Session.LoggedInData -> MP.PostType -> ( Model, Cmd Message )
init key session postType =
    ( { key = key
      , session = session
      , post = Microformats.createEntry
      , postType = postType
      , editor = Editor.create
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
            ( model, MP.createPost SavedPost model.post model.session.micropub )

        SavedPost (Ok url) ->
            ( model, Nav.pushUrl model.key (Urls.editPost url) )

        SavedPost (Err _) ->
            ( model, Cmd.none )


updatePost : (Microformats.Item -> Microformats.Item) -> Model -> Model
updatePost f model =
    { model | post = f model.post }


type Field
    = Name
    | Content


supportedFields : MP.PostType -> List Field
supportedFields t =
    case t of
        MP.Note _ ->
            [ Content ]

        MP.Article _ ->
            [ Name, Content ]

        MP.Photo _ ->
            [ Name, Content ]

        MP.Unknown _ _ ->
            []


view : Model -> Skeleton.Details Message
view model =
    { title = "New " ++ MP.postTypeName model.postType
    , body = [ editPost model ]
    , session = model.session
    }


editPost : Model -> Html Message
editPost model =
    let
        isValid =
            True

        fields =
            supportedFields model.postType

        displayField f =
            List.member f fields
    in
    Html.form
        [ class "w-full h-screen flex flex-col"
        , onSubmit
            (if isValid then
                SavePost

             else
                NoOp
            )
        ]
        [ div [ class "flex flex-none flex-row items-baseline" ]
            [ div [ class "flex-grow" ]
                [ h3 [] [ text ("New " ++ MP.postTypeName model.postType) ]
                ]
            , button
                [ type_ "submit"
                , class "px-3 py-2 mx-1 rounded"
                , if isValid then
                    class "font-bold bg-blue-dark text-white"

                  else
                    class "bg-grey-lightest text-grey"
                ]
                [ text "Save" ]
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
        ]
