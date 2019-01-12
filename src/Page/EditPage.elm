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
import View.Alert as Alert exposing (Alert, Alerts)
import View.Button as Button


type alias Model =
    { session : Session.LoggedInData
    , alerts : Alerts Message
    , path : String
    , originalPage : Maybe Page.Page
    , page : Maybe Page.Page
    , isSaving : Bool
    , editor : Editor.State
    }


init : Session.LoggedInData -> String -> ( Model, Cmd Message )
init session path =
    ( { session = session
      , alerts = Alert.empty DismissAlert
      , path = path
      , originalPage = Nothing
      , page = Nothing
      , isSaving = False
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
    | DismissAlert Alert
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

        DismissAlert alert ->
            ( { model | alerts = Alert.dismiss alert model.alerts }, Cmd.none )

        GotPage (Ok page) ->
            ( { model | originalPage = Just page, page = Just page }, Cmd.none )

        GotPage (Err err) ->
            ( { model | alerts = Alert.append (Alert.fromHttpError err) model.alerts }
            , Cmd.none
            )

        SetName name ->
            ( updatePage (\p -> { p | name = name }) model, Cmd.none )

        SetContent content ->
            ( updatePage (\p -> { p | content = content }) model, Cmd.none )

        SetEditorState editor ->
            ( { model | editor = editor }, Cmd.none )

        SavePage ->
            case model.page of
                Just page ->
                    ( { model | isSaving = True }
                    , Page.update SavedPage page model.session.micropub
                    )

                Nothing ->
                    ( model, Cmd.none )

        SavedPage (Ok _) ->
            ( { model | originalPage = model.page, isSaving = False }, Cmd.none )

        SavedPage (Err err) ->
            ( { model
                | isSaving = False
                , alerts = Alert.append (Alert.fromHttpError err) model.alerts
              }
            , Cmd.none
            )

        RevertPage ->
            ( { model | page = model.originalPage }, Cmd.none )


updatePage : (Page.Page -> Page.Page) -> Model -> Model
updatePage f model =
    { model | page = Maybe.map f model.page }


view : Model -> Skeleton.Details Message
view model =
    let
        title =
            case model.page of
                Just page ->
                    "Edit Page: " ++ page.name

                Nothing ->
                    "Edit Page"
    in
    { title = title
    , body =
        [ case model.page of
            Nothing ->
                loading

            Just page ->
                editPage model page
        ]
    , session = model.session
    , selection = Skeleton.Page model.path
    , alerts = model.alerts
    }


loading : Html Message
loading =
    div [ class "w-full h-screen flex flex-row items-center text-center" ]
        [ h1 [ class "flex-grow font-normal text-orange-light text-3xl" ]
            [ i [ class "fas fa-spinner fa-spin mr-3 text-orange-lighter" ] []
            , text "Loading page..."
            ]
        ]


editPage : Model -> Page.Page -> Html Message
editPage model page =
    let
        changed =
            hasChanges model

        saveState =
            if model.isSaving then
                Button.Working

            else if changed then
                Button.Enabled

            else
                Button.Disabled

        url =
            model.session.micropub.token.me ++ Page.shortPath page
    in
    Html.form
        [ class "w-full h-screen min-h-0 flex flex-col"
        , onSubmit
            (if saveState == Button.Enabled then
                SavePage

             else
                NoOp
            )
        ]
        [ div [ class "flex flex-none flex-row items-baseline" ]
            [ div [ class "flex-grow" ]
                [ strong [ class "text-orange-darker" ]
                    [ i [ class "fas fa-link mr-1" ] [] ]
                , a
                    [ href url
                    , class "text-orange-dark no-underline"
                    , target "_blank"
                    ]
                    [ text url ]
                ]
            , Button.save saveState
            , Button.revert RevertPage changed
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
        , div [ class "flex flex-col flex-grow mt-3 min-h-0" ]
            [ Editor.view
                page.content
                { onInput = SetContent
                , onStateChange = SetEditorState
                , attrs = [ class "w-full flex-grow" ]
                , showCharacterCount = False
                }
                model.editor
            ]
        ]
