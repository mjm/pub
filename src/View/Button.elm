module View.Button exposing (State(..), revert, save)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type State
    = Disabled
    | Enabled
    | Working


save : State -> Html msg
save state =
    let
        iconClass =
            if state == Working then
                "fa-spinner fa-spin"

            else
                "fa-check-circle"
    in
    button
        [ type_ "submit"
        , class "px-3 py-2 mx-1 rounded flex-no-shrink"
        , if state == Disabled then
            class "bg-grey-lightest text-grey"

          else
            class "font-bold bg-blue-dark text-white"
        ]
        [ i [ class ("fas mr-1 " ++ iconClass) ] []
        , text "Save"
        ]


revert : msg -> Bool -> Html msg
revert msg enabled =
    let
        extraAttrs =
            if enabled then
                [ onClick msg ]

            else
                []
    in
    button
        ([ type_ "button"
         , class "px-3 py-2 mx-1 rounded flex-no-shrink"
         , if enabled then
            class "font-semibold bg-grey-lighter text-grey-darker"

           else
            class "bg-grey-lightest text-grey"
         ]
            ++ extraAttrs
        )
        [ text "Revert" ]
