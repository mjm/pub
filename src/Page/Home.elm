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
import Micropub as MP
import Session


type alias Model =
    { session : Session.LoggedInData
    , config : Maybe MP.Config
    }


init : Session.LoggedInData -> ( Model, Cmd Message )
init session =
    ( { session = session
      , config = Nothing
      }
    , MP.getConfig session.micropub GotConfig
    )


type Message
    = NoOp
    | GotConfig (Result Http.Error MP.Config)


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotConfig (Ok cfg) ->
            ( { model | config = Just cfg }, Cmd.none )

        GotConfig (Err _) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Message
view model =
    { title = "It's Bloggin' Time!"
    , body =
        [ div [ class "flex h-screen" ]
            [ nav [ class "flex flex-col w-1/4 xl:w-1/5 py-2 min-h-screen bg-orange-lightest shadow-md z-30" ]
                [ navHeader "Posts"
                , div [ class "flex-row overflow-auto" ]
                    [ ul [ class "list-reset text-sm" ]
                        [ li [ class "text-orange-darkest m-4 truncate" ] [ text "This is a post about some stuff that happened one time" ]
                        ]
                    ]
                , navHeader "Pages"
                , navHeader "Templates"
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
            , class "text-orange-darker no-underline block px-3 pb-1 pt-2 mx-3 my-1 text-xs bg-white border-b-4 border-orange-light shadow-md"
            ]
            [ h3 [ class "font-semibold" ] [ text title ] ]
        ]
