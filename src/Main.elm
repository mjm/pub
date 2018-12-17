port module Main exposing (main)

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
import Page.EditPost as EditPost
import Page.Home as Home
import Page.Login as Login
import Page.NewPost as NewPost
import Session
import Skeleton
import Url
import Url.Builder as UB
import Url.Parser exposing ((</>), (<?>), Parser, map, parse, s, string, top)
import Url.Parser.Query as Query


port storePageData : E.Value -> Cmd msg


port storeSession : E.Value -> Cmd msg


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound Session.Data
    | Login Login.Model
    | Home Home.Model
    | EditPost EditPost.Model
    | NewPost NewPost.Model


type alias Flags =
    { session : E.Value
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init flags url key =
    let
        session =
            Result.withDefault
                Session.empty
                (D.decodeValue Session.decoder flags.session)
    in
    stepUrl url
        { key = key
        , page = NotFound session
        }


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


view : Model -> Browser.Document Message
view model =
    case model.page of
        NotFound _ ->
            { title = "Not Found"
            , body =
                [ h1 [] [ text "Not Found" ]
                , p [] [ text "Nothing found at this URL." ]
                ]
            }

        Login login ->
            mapDocument LoginMsg (Login.view login)

        Home home ->
            Skeleton.view HomeMsg (Home.view home)

        EditPost edit ->
            Skeleton.view EditPostMsg (EditPost.view edit)

        NewPost new ->
            Skeleton.view NewPostMsg (NewPost.view new)


mapDocument : (a -> msg) -> Browser.Document a -> Browser.Document msg
mapDocument f doc =
    { title = doc.title
    , body = List.map (Html.map f) doc.body
    }


type Message
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoginMsg Login.Message
    | HomeMsg Home.Message
    | EditPostMsg EditPost.Message
    | NewPostMsg NewPost.Message


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


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ map HomeRoute top
        , map (LoginRoute Nothing) (s "login")
        , map LoginRoute (s "callback" <?> callbackParamsParser)
        , map EditPostRoute (s "posts" </> s "edit" <?> Query.string "url")
        , map NewPostRoute (s "posts" </> s "new" <?> Query.string "type")
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
                , storePageData = storePageData
                , storeSession = storeSession
                , key = model.key
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
