module Page.Login exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import IndieAuth as Auth
import Json.Encode as E
import Micropub as MP
import Micropub.Html as MPH
import Ports
import Session
import Url.Builder as UB


type alias Model =
    { key : Nav.Key
    , rootUrl : String
    , session : Session.Data
    , callback : Maybe Auth.Callback
    , siteUrl : String
    , loggingIn : Bool
    , micropub : Maybe MP.Session
    }


type alias Flags =
    { session : Session.Data
    , callback : Maybe Auth.Callback
    , key : Nav.Key
    , rootUrl : String
    }


init : Flags -> ( Model, Cmd Message )
init flags =
    let
        loggingIn =
            case flags.callback of
                Nothing ->
                    False

                Just _ ->
                    True
    in
    ( { key = flags.key
      , rootUrl = flags.rootUrl
      , session = flags.session
      , callback = flags.callback
      , siteUrl = ""
      , loggingIn = loggingIn
      , micropub = Nothing
      }
    , initCommand flags.rootUrl flags.callback flags.session
    )


initCommand : String -> Maybe Auth.Callback -> Session.Data -> Cmd Message
initCommand client callback session =
    case session of
        Session.LoggingIn pd ->
            case ( pd.tokenEndpoint, callback ) of
                ( Just url, Just cb ) ->
                    Auth.authorizeToken GotAuthToken url client cb

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


type Message
    = NoOp
    | SetSiteUrl String
    | Login
    | GotPageData (Result Http.Error MPH.Data)
    | GotAuthToken (Result Http.Error Auth.AuthorizedToken)
    | GotConfig MP.Session (Result Http.Error MP.Config)


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSiteUrl url ->
            ( { model | siteUrl = url }, Cmd.none )

        Login ->
            ( { model | loggingIn = True }, MPH.load GotPageData model.siteUrl )

        GotPageData (Ok pd) ->
            ( { model | session = Session.LoggingIn pd }
            , case pd.authorizationEndpoint of
                Nothing ->
                    Cmd.none

                Just endpoint ->
                    Cmd.batch
                        [ loadAuthPage model.rootUrl endpoint model.siteUrl
                        , Ports.storePageData (MPH.encodeLocal pd)
                        ]
            )

        GotPageData (Err _) ->
            ( model, Cmd.none )

        GotAuthToken (Ok token) ->
            setMicropubSession model token

        GotAuthToken (Err _) ->
            ( model, Cmd.none )

        GotConfig mp (Ok config) ->
            let
                newSession =
                    Session.login mp config model.session
            in
            ( { model | session = newSession }
            , Cmd.batch
                [ Ports.storeSession (Session.encode newSession)
                , Nav.pushUrl model.key "/"
                ]
            )

        GotConfig _ (Err _) ->
            ( model, Cmd.none )


loadAuthPage : String -> String -> String -> Cmd msg
loadAuthPage client endpoint url =
    Auth.begin
        { clientId = client
        , redirectUri = client ++ "callback"
        , me = url
        , scopes = [ "create", "update", "delete" ]
        }
        endpoint


setMicropubSession : Model -> Auth.AuthorizedToken -> ( Model, Cmd Message )
setMicropubSession model token =
    let
        micropub =
            case model.session of
                Session.LoggingIn pd ->
                    case pd.micropubEndpoint of
                        Just url ->
                            Just (MP.login url token)

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
    in
    ( { model | micropub = micropub }
    , case micropub of
        Just mp ->
            MP.getConfig (GotConfig mp) mp

        Nothing ->
            Cmd.none
    )


view : Model -> Browser.Document Message
view model =
    { title = "Login"
    , body =
        [ div [ class "w-1/3 bg-white mx-auto mt-16 p-6 shadow-md rounded-lg" ]
            (if model.loggingIn then
                loggingInView model

             else
                loginFormView model
            )
        ]
    }


loggingInView : Model -> List (Html Message)
loggingInView model =
    [ header []
        [ h1 [ class "text-orange-darkest font-light text-center" ]
            [ text "Logging in..." ]
        ]
    ]


loginFormView : Model -> List (Html Message)
loginFormView model =
    [ header []
        [ h1 [ class "text-orange-darkest font-light text-center" ] [ text "Log in to your blog" ] ]
    , p [ class "text-center text-grey-darker text-sm my-4" ]
        [ text "Enter your domain to log in using IndieAuth." ]
    , Html.form [ onSubmit Login ]
        [ div [ class "flex items-center py-2 border-orange border-b" ]
            [ input
                [ class "appearance-none w-full bg-transparent border-none focus:outline-none"
                , type_ "text"
                , placeholder "https://example.com"
                , value model.siteUrl
                , autofocus True
                , onInput SetSiteUrl
                ]
                []
            , button
                [ type_ "submit"
                , class "flex-no-shrink bg-orange hover:bg-orange-dark border-4 border-orange hover:border-orange-dark text-white text-md py-1 px-2 rounded"
                ]
                [ text "Login" ]
            ]
        ]
    ]
