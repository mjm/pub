module Page.Home exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import IndieAuth as Auth
import Microformats
import Micropub as MP
import Micropub.Html as MPH
import Session


type alias Model =
    { session : Session.LoggedInData
    , config : Maybe MP.Config
    , pageData : Maybe MPH.Data
    }


init : Session.LoggedInData -> ( Model, Cmd Message )
init session =
    ( { session = session
      , config = Nothing
      , pageData = Nothing
      }
    , Cmd.batch
        [ MP.getConfig session.micropub GotConfig
        , MPH.load GotPageData session.micropub.token.me
        ]
    )


type Message
    = NoOp
    | GotConfig (Result Http.Error MP.Config)
    | GotPageData (Result Http.Error MPH.Data)


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotConfig (Ok cfg) ->
            ( { model | config = Just cfg }, Cmd.none )

        GotConfig (Err _) ->
            ( model, Cmd.none )

        GotPageData (Ok data) ->
            ( { model | pageData = Just data }, Cmd.none )

        GotPageData (Err _) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Message
view model =
    { title = "It's Bloggin' Time!"
    , body =
        [ div [ class "flex h-screen" ]
            [ nav [ class "flex flex-col w-1/4 xl:w-1/5 min-h-screen bg-orange-lightest shadow-lg z-30 pt-2" ]
                [ navHeader "Posts"
                , div [ class "flex-row" ]
                    [ sidebarPosts model ]
                , navHeader "Pages"
                , p [ class "text-orange-darkest m-3 text-sm" ] [ text "No pages" ]
                , navHeader "Templates"
                , p [ class "text-orange-darkest m-3 text-sm" ] [ text "No templates" ]
                ]
            , div [ class "flex flex-col w-3/4 xl:w-4/5 bg-white p-4" ]
                (case model.config of
                    Nothing ->
                        [ p [] [ text "Loading post types..." ] ]

                    Just cfg ->
                        [ p [] [ text "This blog supports the following post types:" ]
                        , ul [ class "list-reset flex mt-4" ]
                            (List.map
                                (\t ->
                                    li []
                                        [ button [ class "text-sm font-bold bg-blue-dark text-white px-3 py-2 mx-2 rounded" ]
                                            [ text (MP.postTypeName t) ]
                                        ]
                                )
                                (MP.postTypes cfg)
                            )
                        ]
                )
            ]
        ]
    }


navHeader : String -> Html Message
navHeader title =
    div [ class "flex-row" ]
        [ a
            [ href "#"
            , onClick NoOp
            , class "mt-2 uppercase text-orange no-underline block px-3 text-xs"
            ]
            [ h4 [ class "font-bold" ] [ text title ] ]
        ]


sidebarPosts : Model -> Html Message
sidebarPosts model =
    case model.pageData of
        Nothing ->
            p [] [ text "Loading posts..." ]

        Just pd ->
            ul [ class "list-reset text-sm" ] <|
                List.map sidebarPost
                    pd.entries


sidebarPost : Microformats.Item -> Html Message
sidebarPost item =
    li [ class "text-orange-darkest m-3 truncate" ]
        [ text (Maybe.withDefault "Untitled" (Microformats.string "name" item)) ]
