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
import Html.Events.Extra exposing (onClickPreventDefault)
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
    , editingPost : Maybe Microformats.Item
    }


init : Session.LoggedInData -> ( Model, Cmd Message )
init session =
    ( { session = session
      , config = Nothing
      , pageData = Nothing
      , editingPost = Nothing
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
    | EditPost Microformats.Item
    | GotPostToEdit (Result Http.Error Microformats.Item)


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

        EditPost item ->
            case Microformats.string "url" item of
                Just url ->
                    ( model, MP.getPost url model.session.micropub GotPostToEdit )

                Nothing ->
                    ( model, Cmd.none )

        GotPostToEdit (Ok item) ->
            ( { model | editingPost = Just item }, Cmd.none )

        GotPostToEdit (Err _) ->
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
                [ div []
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
                , case model.editingPost of
                    Nothing ->
                        text ""

                    Just p ->
                        Html.form []
                            [ textarea [ value (Maybe.withDefault "" (Microformats.string "content" p)) ] [] ]
                ]
            ]
        ]
    }


navHeader : String -> Html Message
navHeader title =
    div [ class "flex-row" ]
        [ a
            [ onClickPreventDefault NoOp
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
        [ a
            [ class "block"
            , onClickPreventDefault (EditPost item)
            , href "#"
            ]
            [ text (Maybe.withDefault "Untitled" (Microformats.string "name" item)) ]
        ]
