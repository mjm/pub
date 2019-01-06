module Skeleton exposing
    ( Details
    , Selection(..)
    , view
    )

import Blog.Page as Page
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Microformats
import Session
import Url.Builder as UB
import Urls


type alias Details msg =
    { title : String
    , body : List (Html msg)
    , session : Session.LoggedInData
    , selection : Selection
    }


type Selection
    = Empty
    | Post String
    | Page String


type alias Messages msg =
    { logout : msg
    , refreshPages : msg
    , refreshPosts : msg
    }


view : Messages msg -> (a -> msg) -> Details a -> Browser.Document msg
view msgs toMsg details =
    { title = details.title ++ " - Pub"
    , body =
        [ div [ class "font-sans flex h-screen" ]
            [ nav [ class "flex flex-col w-1/4 xl:w-1/5 min-h-screen bg-orange-lightest shadow-lg z-30 pt-2 overflow-y-auto" ]
                [ div [ class "flex flex-row text-xs mb-2 px-3 items-baseline" ]
                    [ div [ class "flex-grow text-orange-dark" ]
                        [ text (friendlyMe details) ]
                    , button
                        [ class "bg-orange-lighter px-2 py-1 font-bold text-orange-dark rounded"
                        , onClick msgs.logout
                        ]
                        [ text "Logout" ]
                    ]
                , div [ class "flex flex-row items-baseline" ]
                    [ navHeader "Pages"
                    , button
                        [ class "text-orange px-3"
                        , onClick msgs.refreshPages
                        ]
                        [ i [ class "fas fa-redo-alt" ] [] ]
                    ]
                , div [ class "flex-row" ]
                    [ sidebarPages details ]
                , div [ class "flex flex-row items-baseline" ]
                    [ navHeader "Posts"
                    , button
                        [ class "text-orange px-3"
                        , onClick msgs.refreshPosts
                        ]
                        [ i [ class "fas fa-redo-alt" ] [] ]
                    ]
                , div [ class "flex-row" ]
                    [ sidebarPosts details ]
                , div [ class "flex-row" ]
                    [ navHeader "Templates" ]
                , div [ class "text-orange-darkest m-3 text-sm" ] [ text "No templates" ]
                ]
            , Html.map toMsg <| div [ class "flex flex-col w-3/4 xl:w-4/5 bg-white p-4" ] details.body
            ]
        ]
    }


navHeader : String -> Html msg
navHeader title =
    a
        [ href Urls.home
        , class "no-underline text-orange flex-grow"
        ]
        [ h4 [ class "mt-2 mb-2 uppercase no-underline block px-3 text-xs font-bold" ]
            [ text title ]
        ]


sidebarPosts : Details a -> Html msg
sidebarPosts details =
    ul [ class "list-reset text-sm" ] <|
        List.map
            (sidebarPost details.selection)
            details.session.pageData.entries


sidebarPost : Selection -> Microformats.Item -> Html msg
sidebarPost selection item =
    let
        name =
            Maybe.withDefault "Untitled" (Microformats.string "name" item)

        url =
            Microformats.string "url" item

        hrefUrl =
            case url of
                Nothing ->
                    "#"

                Just u ->
                    Urls.editPost u

        isSelected =
            case ( url, selection ) of
                ( Just u1, Post u2 ) ->
                    u1 == u2

                _ ->
                    False
    in
    sidebarItem name hrefUrl isSelected


sidebarPages : Details a -> Html msg
sidebarPages details =
    ul [ class "list-reset text-sm" ] <|
        List.map (sidebarPage details.selection) details.session.pages


sidebarPage : Selection -> Page.Page -> Html msg
sidebarPage selection page =
    let
        isSelected =
            case selection of
                Page path ->
                    Page.shortPath page == path

                _ ->
                    False
    in
    sidebarItem page.name (Urls.editPage (Page.shortPath page)) isSelected


sidebarItem : String -> String -> Bool -> Html msg
sidebarItem title url isSelected =
    li []
        [ a
            [ class "-mt-1 px-3 pb-2 pt-2 block no-underline truncate"
            , class
                (if isSelected then
                    "text-white bg-orange-dark"

                 else
                    "text-orange-darkest hover:bg-orange-lighter"
                )
            , href url
            ]
            [ text title ]
        ]


friendlyMe : Details a -> String
friendlyMe details =
    let
        me =
            details.session.micropub.token.me

        trimSlash str =
            if String.endsWith "/" str then
                String.dropRight 1 str

            else
                str
    in
    me
        |> String.replace "https://" ""
        |> String.replace "http://" ""
        |> trimSlash
