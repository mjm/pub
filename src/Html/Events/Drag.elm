module Html.Events.Drag exposing
    ( onDragEnter
    , onDragLeave
    , onDragOver
    , onDrop
    )

import File exposing (File)
import Html exposing (..)
import Html.Events exposing (custom)
import Json.Decode as D


onDrop : (List File -> msg) -> Attribute msg
onDrop toMsg =
    custom "drop" <|
        D.map
            (\files ->
                { message = toMsg files, stopPropagation = True, preventDefault = True }
            )
            filesDecoder


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.field "dataTransfer" (D.field "files" (D.list File.decoder))


onDragOver : msg -> Attribute msg
onDragOver msg =
    custom "dragover" <| D.succeed { message = msg, stopPropagation = True, preventDefault = True }


onDragEnter : msg -> Attribute msg
onDragEnter msg =
    custom "dragenter" <| D.succeed { message = msg, stopPropagation = True, preventDefault = True }


onDragLeave : msg -> Attribute msg
onDragLeave msg =
    custom "dragleave" <| D.succeed { message = msg, stopPropagation = True, preventDefault = True }
