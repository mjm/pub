module View.Alert exposing (Alert(..), Alerts, append, dismiss, empty, fromHttpError, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http


type Alerts msg
    = Alerts (Alert -> msg) (List Alert)


type Alert
    = Info String
    | Warning String
    | Error String


empty : (Alert -> msg) -> Alerts msg
empty dismissMsg =
    Alerts dismissMsg []


append : Alert -> Alerts msg -> Alerts msg
append alert (Alerts dismissMsg alerts) =
    Alerts dismissMsg (alerts ++ [ alert ])


dismiss : Alert -> Alerts msg -> Alerts msg
dismiss alert (Alerts dismissMsg alerts) =
    Alerts dismissMsg <|
        List.filter (\x -> alert /= x) alerts


fromHttpError : Http.Error -> Alert
fromHttpError err =
    Error <|
        case err of
            Http.Timeout ->
                "Hmm, we never got a response from the server. Please try again later."

            Http.NetworkError ->
                "Oh no! The network dropped out on us. Please try again!"

            Http.BadStatus 404 ->
                "Hmm, we couldn't find what you were looking for."

            Http.BadStatus 500 ->
                "Oh no! Something bad happened on the server. Please try again later."

            _ ->
                "Uh oh, something bad happened. Please try again later."


view : Alerts msg -> Html msg
view (Alerts dismissMsg alerts) =
    div [ class "absolute mx-auto w-full pin-t z-40 pointer-events-none" ]
        (List.map (showAlert dismissMsg) alerts)


showAlert : (Alert -> msg) -> Alert -> Html msg
showAlert dismissMsg alert =
    let
        ( text, color, icon ) =
            case alert of
                Info msg ->
                    ( msg, "blue", "fa-info-circle" )

                Warning msg ->
                    ( msg, "orange", "fa-exclamation-triangle" )

                Error msg ->
                    ( msg, "red", "fa-times-circle" )

        classes =
            "border-" ++ color ++ " bg-" ++ color ++ "-lightest text-" ++ color ++ "-darkest"
    in
    div
        [ class "w-1/2 mx-auto my-3 border-t-4 px-3 py-3 rounded-b shadow-md flex flex-row items-start pointer-events-auto"
        , class classes
        ]
        [ i [ class ("fas mr-2 text-" ++ color), class icon ] []
        , p [ class "flex-grow" ] [ Html.text text ]
        , button [ onClick (dismissMsg alert) ]
            [ i [ class ("fas fa-times text-" ++ color) ] [] ]
        ]
