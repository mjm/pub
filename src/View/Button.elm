module View.Button exposing (State(..), save)

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
        , class "px-3 py-2 mx-1 rounded"
        , if state == Disabled then
            class "bg-grey-lightest text-grey"

          else
            class "font-bold bg-blue-dark text-white"
        ]
        [ i [ class ("fas mr-1 " ++ iconClass) ] []
        , text "Save"
        ]
