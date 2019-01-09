module View.PostForm exposing (contentField, nameField, photoField)

import Editor
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Microformats
import Micropub.PostType as PostType exposing (PostType(..))
import View.Photos as Photos


type Field
    = Name
    | Content
    | Photo


supportedFields : PostType -> List Field
supportedFields t =
    case t of
        Note _ ->
            [ Content ]

        Article _ ->
            [ Name, Content ]

        PostType.Photo _ ->
            [ Name, Content, Photo ]

        Unknown _ _ ->
            []


displayField : PostType -> Field -> Bool
displayField t f =
    List.member f (supportedFields t)


nameField : (String -> msg) -> PostType -> Microformats.Item -> Html msg
nameField msg pt item =
    if displayField pt Name then
        div [ class "flex-none py-2 border-orange border-b" ]
            [ input
                [ class "px-2 text-xl appearance-none w-full bg-transparent border-none focus:outline-none"
                , placeholder "Untitled"
                , onInput msg
                , value (Maybe.withDefault "" (Microformats.string "name" item))
                ]
                []
            ]

    else
        text ""


contentField :
    { onInput : String -> msg
    , onStateChange : Editor.State -> msg
    }
    -> PostType
    -> Microformats.Item
    -> Editor.State
    -> Html msg
contentField cfg pt item editor =
    if displayField pt Content then
        div [ class "flex flex-col flex-grow mt-3" ]
            [ Editor.view
                (Maybe.withDefault "" (Microformats.string "content" item))
                { onInput = cfg.onInput
                , onStateChange = cfg.onStateChange
                , attrs = [ class "w-full flex-grow" ]
                }
                editor
            ]

    else
        text ""


photoField : (Photos.Message -> msg) -> PostType -> Photos.Model -> Html msg
photoField msg pt photos =
    if displayField pt Photo then
        Photos.view { attrs = [], toMsg = msg } photos

    else
        text ""
