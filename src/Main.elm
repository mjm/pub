port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Http
import IndieAuth as Auth
import Json.Decode as D
import Json.Encode as E
import Micropub as MP
import Page.Home as Home
import Page.Login as Login
import Session
import Url
import Url.Builder as UB
import Url.Parser exposing ((</>), (<?>), Parser, map, parse, s, string, top)
import Url.Parser.Query as Query


port storeCredentials : E.Value -> Cmd msg


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound Session.Data
    | Login Login.Model
    | LoggingIn Session.Data
    | Home Home.Model


type alias Flags =
    { micropubUrl : String
    , credentials : E.Value
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init flags url key =
    let
        tokenResult =
            D.decodeValue Auth.tokenDecoder flags.credentials

        session =
            case tokenResult of
                Ok token ->
                    Session.addToken token Session.empty

                Err _ ->
                    Session.empty
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

        LoggingIn session ->
            session

        Home home ->
            home.session


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

        LoggingIn _ ->
            { title = "Logging In..."
            , body =
                [ h1 [] [ text "Logging In" ]
                , p [] [ text "Authorizing you!" ]
                ]
            }

        Login login ->
            mapDocument LoginMsg (Login.view login)

        Home home ->
            mapDocument HomeMsg (Home.view home)


mapDocument : (a -> msg) -> Browser.Document a -> Browser.Document msg
mapDocument f doc =
    { title = doc.title
    , body = List.map (Html.map f) doc.body
    }


type Message
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotAuthToken (Result Http.Error Auth.AuthorizedToken)
    | LoginMsg Login.Message
    | HomeMsg Home.Message


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

        GotAuthToken (Ok token) ->
            saveToken token model

        GotAuthToken (Err _) ->
            ( model, Cmd.none )

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


saveToken : Auth.AuthorizedToken -> Model -> ( Model, Cmd Message )
saveToken token model =
    ( { model | page = LoggingIn (Session.addToken token (getSession model)) }
    , Cmd.batch
        [ Nav.pushUrl model.key "/"
        , storeCredentials (Auth.encodeToken token)
        ]
    )


stepLogin : Model -> ( Login.Model, Cmd Login.Message ) -> ( Model, Cmd Message )
stepLogin model ( login, cmds ) =
    ( { model | page = Login login }
    , Cmd.map LoginMsg cmds
    )


stepHome : Model -> ( Home.Model, Cmd Home.Message ) -> ( Model, Cmd Message )
stepHome model ( home, cmds ) =
    ( { model | page = Home home }
    , Cmd.map HomeMsg cmds
    )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none


type Route
    = LoginRoute
    | CallbackRoute (Maybe String) (Maybe String) (Maybe String)
    | HomeRoute


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ map HomeRoute top
        , map LoginRoute (s "login")
        , map CallbackRoute (s "callback" <?> Query.string "code" <?> Query.string "me" <?> Query.string "state")
        ]


stepUrl : Url.Url -> Model -> ( Model, Cmd Message )
stepUrl url model =
    let
        session =
            getSession model
    in
    case parse routeParser url of
        Just LoginRoute ->
            stepLogin model (Login.init session)

        Just (CallbackRoute (Just code) (Just me) (Just state)) ->
            ( { model | page = LoggingIn session }
            , Auth.authorizeToken GotAuthToken code me state
            )

        Just HomeRoute ->
            case session.token of
                Nothing ->
                    ( { model | page = NotFound session }, Cmd.none )

                Just token ->
                    stepHome model (Home.init session token)

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
