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
    }


init : Session.LoggedInData -> ( Model, Cmd Message )
init session =
    ( { session = session }
    , Cmd.none
    )


type Message
    = NoOp



{-
   | EditPost Microformats.Item
   | GotPostToEdit (Result Http.Error Microformats.Item)
-}


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



{-
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
-}


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
                [ p [] [ text "This blog supports the following post types:" ]
                , ul [ class "list-reset flex mt-4" ]
                    (List.map
                        (\t ->
                            li []
                                [ button [ class "text-sm font-bold bg-blue-dark text-white px-3 py-2 mx-2 rounded" ]
                                    [ text (MP.postTypeName t) ]
                                ]
                        )
                        (MP.postTypes model.session.config)
                    )
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
    ul [ class "list-reset text-sm" ] <|
        List.map sidebarPost model.session.pageData.entries


sidebarPost : Microformats.Item -> Html Message
sidebarPost item =
    li [ class "text-orange-darkest m-3" ]
        [ a
            [ class "block text-orange-darkest truncate"
            , href "/posts/foo"
            ]
            [ text (Maybe.withDefault "Untitled" (Microformats.string "name" item)) ]
        ]
