module Main exposing (main)

import Blog.Page as Page
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import IndieAuth as Auth
import Json.Decode as D
import Json.Encode as E
import Micropub as MP
import Micropub.Html as MPH
import Page.EditPage as EditPage
import Page.EditPost as EditPost
import Page.Home as Home
import Page.Login as Login
import Page.NewPost as NewPost
import Ports
import Session
import Skeleton
import Url
import Url.Builder as UB
import Url.Parser exposing ((</>), (<?>), Parser, map, parse, s, string, top)
import Url.Parser.Query as Query


type alias Model =
    { key : Nav.Key
    , rootUrl : String
    , page : Page
    }


type Page
    = NotFound Session.Data
    | Login Login.Model
    | Home Home.Model
    | EditPost EditPost.Model
    | NewPost NewPost.Model
    | EditPage EditPage.Model


type alias Flags =
    { rootUrl : String
    , session : E.Value
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init flags url key =
    let
        session =
            Result.withDefault
                Session.empty
                (D.decodeValue Session.decoder flags.session)

        ( model, cmds ) =
            stepUrl url
                { key = key
                , rootUrl = flags.rootUrl
                , page = NotFound session
                }
    in
    ( model
    , case session of
        Session.LoggedIn data ->
            Cmd.batch [ reloadEverything data, cmds ]

        _ ->
            cmds
    )


reloadEverything : Session.LoggedInData -> Cmd Message
reloadEverything data =
    Cmd.batch
        [ MPH.load GotPageData data.micropub.token.me
        , Page.all GotPages data.micropub
        ]


getSession : Model -> Session.Data
getSession model =
    case model.page of
        NotFound session ->
            session

        Login login ->
            login.session

        Home home ->
            Session.LoggedIn home.session

        EditPost edit ->
            Session.LoggedIn edit.session

        NewPost new ->
            Session.LoggedIn new.session

        EditPage edit ->
            Session.LoggedIn edit.session


updateSession : (Session.Data -> Session.Data) -> Model -> Model
updateSession f model =
    let
        newSession =
            f (getSession model)

        newPage =
            case model.page of
                NotFound _ ->
                    NotFound newSession

                Login login ->
                    Login { login | session = newSession }

                Home home ->
                    case newSession of
                        Session.LoggedIn data ->
                            Home { home | session = data }

                        _ ->
                            model.page

                EditPost edit ->
                    case newSession of
                        Session.LoggedIn data ->
                            EditPost { edit | session = data }

                        _ ->
                            model.page

                NewPost new ->
                    case newSession of
                        Session.LoggedIn data ->
                            NewPost { new | session = data }

                        _ ->
                            model.page

                EditPage edit ->
                    case newSession of
                        Session.LoggedIn data ->
                            EditPage { edit | session = data }

                        _ ->
                            model.page
    in
    { model | page = newPage }


view : Model -> Browser.Document Message
view model =
    let
        skeleton =
            Skeleton.view Logout
    in
    case model.page of
        NotFound _ ->
            { title = "Page Not Found - Pub"
            , body = notFoundView
            }

        Login login ->
            mapDocument LoginMsg (Login.view login)

        Home home ->
            skeleton HomeMsg (Home.view home)

        EditPost edit ->
            skeleton EditPostMsg (EditPost.view edit)

        NewPost new ->
            skeleton NewPostMsg (NewPost.view new)

        EditPage edit ->
            skeleton EditPageMsg (EditPage.view edit)


notFoundView : List (Html Message)
notFoundView =
    [ div [ class "w-1/3 bg-white mx-auto mt-16 p-6 shadow-md rounded-lg text-orange-darkest text-center" ]
        [ h1 [ class "font-light" ] [ text "Page Not Found" ]
        , p [ class "mt-3" ] [ text "Nothing found at this URL." ]
        ]
    ]


mapDocument : (a -> msg) -> Browser.Document a -> Browser.Document msg
mapDocument f doc =
    { title = doc.title ++ " - Pub"
    , body = List.map (Html.map f) doc.body
    }


type Message
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotPageData (Result Http.Error MPH.Data)
    | GotPages (Result Http.Error (List Page.Page))
    | Logout
    | LoginMsg Login.Message
    | HomeMsg Home.Message
    | EditPostMsg EditPost.Message
    | NewPostMsg NewPost.Message
    | EditPageMsg EditPage.Message


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        GotPageData (Ok pd) ->
            updatePageData pd model

        GotPageData (Err _) ->
            ( model, Cmd.none )

        GotPages (Ok pages) ->
            updatePages pages model

        GotPages (Err _) ->
            ( model, Cmd.none )

        Logout ->
            ( model
            , Cmd.batch
                [ Ports.clearSession ()
                , Nav.pushUrl model.key "/login"
                ]
            )

        LoginMsg msg ->
            case model.page of
                Login login ->
                    stepLogin model (Login.update msg login)

                _ ->
                    ( model, Cmd.none )

        HomeMsg msg ->
            case model.page of
                Home home ->
                    stepHome model (Home.update msg home)

                _ ->
                    ( model, Cmd.none )

        EditPostMsg msg ->
            case model.page of
                EditPost edit ->
                    stepEditPost model (EditPost.update msg edit)

                _ ->
                    ( model, Cmd.none )

        NewPostMsg msg ->
            case model.page of
                NewPost new ->
                    stepNewPost model (NewPost.update msg new)

                _ ->
                    ( model, Cmd.none )

        EditPageMsg msg ->
            case model.page of
                EditPage edit ->
                    stepEditPage model (EditPage.update msg edit)

                _ ->
                    ( model, Cmd.none )


updatePageData : MPH.Data -> Model -> ( Model, Cmd Message )
updatePageData pd model =
    ( updateSession (Session.updatePageData pd) model
    , Ports.storePageData (E.object [ ( "pageData", MPH.encodeLocal pd ) ])
    )


updatePages : List Page.Page -> Model -> ( Model, Cmd Message )
updatePages pages model =
    let
        newModel =
            updateSession (Session.updatePages pages) model
    in
    ( newModel
    , Ports.storeSession (Session.encode (getSession newModel))
    )


stepLogin : Model -> ( Login.Model, Cmd Login.Message ) -> ( Model, Cmd Message )
stepLogin =
    stepPage Login LoginMsg


stepHome : Model -> ( Home.Model, Cmd Home.Message ) -> ( Model, Cmd Message )
stepHome =
    stepPage Home HomeMsg


stepEditPost : Model -> ( EditPost.Model, Cmd EditPost.Message ) -> ( Model, Cmd Message )
stepEditPost =
    stepPage EditPost EditPostMsg


stepNewPost : Model -> ( NewPost.Model, Cmd NewPost.Message ) -> ( Model, Cmd Message )
stepNewPost =
    stepPage NewPost NewPostMsg


stepEditPage =
    stepPage EditPage EditPageMsg


stepPage : (model -> Page) -> (msg -> Message) -> Model -> ( model, Cmd msg ) -> ( Model, Cmd Message )
stepPage toPage toMsg model ( pageModel, cmds ) =
    ( { model | page = toPage pageModel }
    , Cmd.map toMsg cmds
    )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none


type Route
    = LoginRoute (Maybe Auth.Callback)
    | HomeRoute
    | EditPostRoute (Maybe String)
    | NewPostRoute (Maybe String)
    | EditPageRoute (Maybe String)


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ map HomeRoute top
        , map (LoginRoute Nothing) (s "login")
        , map LoginRoute (s "callback" <?> callbackParamsParser)
        , map EditPostRoute (s "posts" </> s "edit" <?> Query.string "url")
        , map NewPostRoute (s "posts" </> s "new" <?> Query.string "type")
        , map EditPageRoute (s "pages" </> s "edit" <?> Query.string "path")
        ]


callbackParamsParser : Query.Parser (Maybe Auth.Callback)
callbackParamsParser =
    Query.map3
        (\c m s ->
            case ( c, m, s ) of
                ( Just code, Just me, Just state ) ->
                    Just (Auth.Callback code me state)

                _ ->
                    Nothing
        )
        (Query.string "code")
        (Query.string "me")
        (Query.string "state")


stepUrl : Url.Url -> Model -> ( Model, Cmd Message )
stepUrl url model =
    let
        session =
            getSession model

        requireLoggedIn next =
            case session of
                Session.LoggedIn sess ->
                    next sess

                _ ->
                    ( { model | page = NotFound session }, Nav.pushUrl model.key "/login" )
    in
    case parse routeParser url of
        Just (LoginRoute callback) ->
            Login.init
                { session = session
                , callback = callback
                , key = model.key
                , rootUrl = model.rootUrl
                }
                |> stepLogin model

        Just HomeRoute ->
            requireLoggedIn
                (\sess -> stepHome model (Home.init sess))

        Just (EditPostRoute (Just u)) ->
            requireLoggedIn
                (\sess -> stepEditPost model (EditPost.init sess u))

        Just (NewPostRoute typeKey) ->
            requireLoggedIn
                (\sess ->
                    let
                        postType =
                            MP.getPostType typeKey sess.config
                    in
                    case postType of
                        Just t ->
                            stepNewPost model (NewPost.init model.key sess t)

                        Nothing ->
                            ( { model | page = NotFound session }, Cmd.none )
                )

        Just (EditPageRoute (Just path)) ->
            requireLoggedIn
                (\sess -> stepEditPage model (EditPage.init sess path))

        _ ->
            ( { model | page = NotFound session }, Cmd.none )


main : Program Flags Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
