module Editor exposing (State, create, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Markdown


type alias Config msg =
    { onInput : String -> msg
    , onStateChange : State -> msg
    , attrs : List (Attribute msg)
    }


type State
    = Editing
    | Previewing


create : State
create =
    Editing


view : String -> Config msg -> State -> Html msg
view input config state =
    div (config.attrs ++ [ class "flex flex-col" ])
        [ ul [ class "list-reset flex border-b" ]
            [ tabItem config.onStateChange state Editing "Write"
            , tabItem config.onStateChange state Previewing "Preview"
            ]
        , case state of
            Editing ->
                textarea
                    [ onInput config.onInput
                    , value input
                    , class "flex-grow focus:outline-none leading-normal border-l border-r border-b p-3"
                    ]
                    []

            Previewing ->
                div [ class "flex-grow leading-normal border-l border-r border-b p-3 overflow-y-auto" ]
                    [ Markdown.toHtml [ class "preview-content" ] input ]
        ]


tabItem : (State -> msg) -> State -> State -> String -> Html msg
tabItem toMsg currentState state title =
    li [ class "-mb-px mr-1" ]
        [ button
            ([ type_ "button"
             , class "font-semibold inline-block bg-white py-2 px-4 text-orange-dark"
             , class
                (if currentState == state then
                    "rounded-t border-l border-r border-t"

                 else
                    ""
                )
             ]
                ++ (if currentState == state then
                        []

                    else
                        [ onClick (toMsg state) ]
                   )
            )
            [ text title ]
        ]
