module Skeleton exposing (Details, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Microformats
import Session
import Url.Builder as UB


type alias Details msg =
    { title : String
    , body : List (Html msg)
    , session : Session.LoggedInData
    }


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title
    , body =
        [ div [ class "flex h-screen" ]
            [ nav [ class "flex flex-col w-1/4 xl:w-1/5 min-h-screen bg-orange-lightest shadow-lg z-30 pt-2" ]
                [ navHeader "Posts"
                , div [ class "flex-row" ]
                    [ sidebarPosts details.session ]
                , navHeader "Pages"
                , p [ class "text-orange-darkest m-3 text-sm" ] [ text "No pages" ]
                , navHeader "Templates"
                , p [ class "text-orange-darkest m-3 text-sm" ] [ text "No templates" ]
                ]
            , Html.map toMsg <| div [ class "flex flex-col w-3/4 xl:w-4/5 bg-white p-4" ] details.body
            ]
        ]
    }


navHeader : String -> Html msg
navHeader title =
    div [ class "flex-row" ]
        [ h4 [ class "mt-2 uppercase text-orange no-underline block px-3 text-xs font-bold" ]
            [ text title ]
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
    li [ class "text-orange-darkest m-3" ]
        [ case url of
            Nothing ->
                p [] [ text name ]

            Just u ->
                a
                    [ class "block text-orange-darkest truncate"
                    , href (editPostUrl u)
                    ]
                    [ text name ]
        ]


editPostUrl : String -> String
editPostUrl postUrl =
    "/posts/edit" ++ UB.toQuery [ UB.string "url" postUrl ]
