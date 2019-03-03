module Page.NewPage exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Blog.Page as Page
import Browser
import Browser.Navigation as Nav
import Editor
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Micropub.Html as MPH
import Session
import Skeleton
import Urls
import View.Alert as Alert exposing (Alert, Alerts)
import View.Button as Button


type alias Model =
    { key : Nav.Key
    , session : Session.LoggedInData
    , alerts : Alerts Message
    , page : Page.Page
    , isSaving : Bool
    , editor : Editor.State
    }


init : Nav.Key -> Session.LoggedInData -> ( Model, Cmd Message )
init key session =
    ( { key = key
      , session = session
      , alerts = Alert.empty DismissAlert
      , page = Page.empty
      , isSaving = False
      , editor = Editor.create
      }
    , Cmd.none
    )


type Message
    = NoOp
    | DismissAlert Alert
    | SetPath String
    | SetName String
    | SetContent String
    | SetEditorState Editor.State
    | SavePage
    | SavedPage (Result Http.Error ())


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DismissAlert alert ->
            ( { model | alerts = Alert.dismiss alert model.alerts }, Cmd.none )

        SetPath path ->
            ( updatePage (Page.setShortPath path) model, Cmd.none )

        SetName name ->
            ( updatePage (\p -> { p | name = name }) model, Cmd.none )

        SetContent content ->
            ( updatePage (\p -> { p | content = content }) model, Cmd.none )

        SetEditorState editor ->
            ( { model | editor = editor }, Cmd.none )

        SavePage ->
            ( { model | isSaving = True }
            , Page.update SavedPage model.page model.session.micropub
            )

        SavedPage (Ok ()) ->
            ( { model | isSaving = False }
            , Nav.pushUrl model.key (Urls.editPage (Page.shortPath model.page))
            )

        SavedPage (Err err) ->
            ( { model
                | isSaving = False
                , alerts = Alert.append (Alert.fromHttpError err) model.alerts
              }
            , Cmd.none
            )


updatePage : (Page.Page -> Page.Page) -> Model -> Model
updatePage f model =
    { model | page = f model.page }


view : Model -> Skeleton.Details Message
view model =
    { title = "New Page"
    , body = [ editPage model ]
    , session = model.session
    , selection = Skeleton.Empty
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


editPage : Model -> Html Message
editPage model =
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

        url =
            model.session.micropub.token.me
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
            [ div [ class "flex flex-grow items-baseline" ]
                [ strong [ class "text-orange-darker" ]
                    [ i [ class "fas fa-link mr-1" ] [] ]
                , a
                    [ href (url ++ Page.shortPath model.page)
                    , class "text-orange-dark no-underline"
                    , target "_blank"
                    ]
                    [ text url ]
                , input
                    [ class "text-orange-dark flex-grow"
                    , placeholder "my-new-page"
                    , onInput SetPath
                    , value (Page.shortPath model.page)
                    ]
                    []
                ]
            , Button.save saveState
            ]
        , div [ class "flex-none py-2 border-orange border-b" ]
            [ input
                [ class "px-2 text-xl appearance-none w-full bg-transparent border-none focus:outline-none"
                , placeholder "Untitled"
                , onInput SetName
                , value model.page.name
                ]
                []
            ]
        , div [ class "flex flex-col flex-grow mt-3 min-h-0" ]
            [ Editor.view
                model.page.content
                { onInput = SetContent
                , onStateChange = SetEditorState
                , attrs = [ class "w-full flex-grow" ]
                , showCharacterCount = False
                }
                model.editor
            ]
        ]
