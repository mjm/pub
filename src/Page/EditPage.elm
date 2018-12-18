module Page.EditPage exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Blog.Page as Page
import Browser
import Editor
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Micropub.Html as MPH
import Session
import Skeleton


type alias Model =
    { session : Session.LoggedInData
    , originalPage : Maybe Page.Page
    , page : Maybe Page.Page
    , editor : Editor.State
    }


init : Session.LoggedInData -> String -> ( Model, Cmd Message )
init session path =
    ( { session = session
      , originalPage = Nothing
      , page = Nothing
      , editor = Editor.create
      }
    , Page.get GotPage path session.micropub
    )


hasChanges : Model -> Bool
hasChanges model =
    case ( model.originalPage, model.page ) of
        ( Just o, Just n ) ->
            Page.hasChanges o n

        _ ->
            False


type Message
    = NoOp
    | GotPage (Result Http.Error Page.Page)
    | SetName String
    | SetContent String
    | SetEditorState Editor.State
    | SavePage
    | SavedPage (Result Http.Error ())
    | RevertPage


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPage (Ok page) ->
            ( { model | originalPage = Just page, page = Just page }, Cmd.none )

        GotPage (Err _) ->
            ( model, Cmd.none )

        SetName name ->
            ( updatePage (\p -> { p | name = name }) model, Cmd.none )

        SetContent content ->
            ( updatePage (\p -> { p | content = content }) model, Cmd.none )

        SetEditorState editor ->
            ( { model | editor = editor }, Cmd.none )

        SavePage ->
            case model.page of
                Just page ->
                    ( model, Page.update SavedPage page model.session.micropub )

                Nothing ->
                    ( model, Cmd.none )

        SavedPage (Ok _) ->
            ( { model | originalPage = model.page }, Cmd.none )

        SavedPage (Err _) ->
            ( model, Cmd.none )

        RevertPage ->
            ( { model | page = model.originalPage }, Cmd.none )


updatePage : (Page.Page -> Page.Page) -> Model -> Model
updatePage f model =
    { model | page = Maybe.map f model.page }


view : Model -> Skeleton.Details Message
view model =
    { title = "It's Bloggin' Time!"
    , body =
        [ case model.page of
            Nothing ->
                p [] [ text "Loading page to edit..." ]

            Just page ->
                editPage model page
        ]
    , session = model.session
    }


editPage : Model -> Page.Page -> Html Message
editPage model page =
    let
        changed =
            hasChanges model

        url =
            model.session.micropub.token.me ++ Page.shortPath page
    in
    Html.form
        [ class "w-full h-screen flex flex-col"
        , onSubmit
            (if changed then
                SavePage

             else
                NoOp
            )
        ]
        [ div [ class "flex flex-none flex-row items-baseline" ]
            [ div [ class "flex-grow" ]
                [ strong [ class "text-orange-darkest" ] [ text "Permalink: " ]
                , a
                    [ href url
                    , class "text-orange-dark"
                    , target "_blank"
                    ]
                    [ text url ]
                ]
            , button
                [ type_ "submit"
                , class "px-3 py-2 mx-1 rounded"
                , if changed then
                    class "font-bold bg-blue-dark text-white"

                  else
                    class "bg-grey-lightest text-grey"
                ]
                [ text "Save" ]
            , button
                [ type_ "button"
                , onClick
                    (if changed then
                        RevertPage

                     else
                        NoOp
                    )
                , class "font-semibold px-3 py-2 mx-1 rounded"
                , if changed then
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
                , value page.name
                ]
                []
            ]
        , div [ class "flex flex-col flex-grow mt-3" ]
            [ Editor.view
                page.content
                { onInput = SetContent
                , onStateChange = SetEditorState
                , attrs = [ class "w-full flex-grow" ]
                }
                model.editor
            ]
        ]
