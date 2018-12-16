module Page.EditPost exposing
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
    , post : Maybe Microformats.Item
    }


init : Session.LoggedInData -> String -> ( Model, Cmd Message )
init session url =
    ( { session = session
      , post = Nothing
      }
    , MP.getPost GotPost url session.micropub
    )


type Message
    = NoOp
    | GotPost (Result Http.Error Microformats.Item)


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPost (Ok post) ->
            ( { model | post = Just post }, Cmd.none )

        GotPost (Err _) ->
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
                [ case model.post of
                    Nothing ->
                        p [] [ text "Loading post to edit..." ]

                    Just post ->
                        p [] [ text <| Maybe.withDefault "" (Microformats.string "content" post) ]
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
