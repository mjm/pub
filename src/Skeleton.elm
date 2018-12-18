module Skeleton exposing (Details, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Microformats
import Session
import Url.Builder as UB
import Urls


type alias Details msg =
    { title : String
    , body : List (Html msg)
    , session : Session.LoggedInData
    }


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title
    , body =
        [ div [ class "font-sans flex h-screen" ]
            [ nav [ class "flex flex-col w-1/4 xl:w-1/5 min-h-screen bg-orange-lightest shadow-lg z-30 pt-2 overflow-y-auto" ]
                [ navHeader "Posts"
                , div [ class "flex-row" ]
                    [ sidebarPosts details.session ]
                , navHeader "Pages"
                , div [ class "text-orange-darkest m-3 text-sm" ] [ text "No pages" ]
                , navHeader "Templates"
                , div [ class "text-orange-darkest m-3 text-sm" ] [ text "No templates" ]
                ]
            , Html.map toMsg <| div [ class "flex flex-col w-3/4 xl:w-4/5 bg-white p-4" ] details.body
            ]
        ]
    }


navHeader : String -> Html msg
navHeader title =
    div [ class "flex-row" ]
        [ a
            [ href Urls.home
            , class "no-underline text-orange"
            ]
            [ h4 [ class "mt-2 mb-2 uppercase no-underline block px-3 text-xs font-bold" ]
                [ text title ]
            ]
        ]


sidebarPosts : Session.LoggedInData -> Html msg
sidebarPosts session =
    ul [ class "list-reset text-sm" ] <|
        List.map sidebarPost session.pageData.entries


sidebarPost : Microformats.Item -> Html msg
sidebarPost item =
    let
        name =
            Maybe.withDefault "Untitled" (Microformats.string "name" item)

        url =
            Microformats.string "url" item
    in
    li [ class "text-orange-darkest" ]
        [ case url of
            Nothing ->
                p [ class "p-1" ] [ text name ]

            Just u ->
                a
                    [ class "-mt-1 px-3 pb-2 pt-2 block no-underline text-orange-darkest truncate hover:bg-orange-lighter"
                    , href (Urls.editPost u)
                    ]
                    [ text name ]
        ]
