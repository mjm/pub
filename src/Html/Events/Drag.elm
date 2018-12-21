module Html.Events.Drag exposing (onDrop, onDragOver, onDragEnter, onDragLeave)

{-| This module provides extra HTML event handlers for HTML5 drag and drop
events.

This is not a complete set of appropriate events, but instead just the set that
I need in order to support uploading files.

@docs onDrop, onDragOver, onDragEnter, onDragLeave

-}

import File exposing (File)
import Html exposing (..)
import Html.Events exposing (custom)
import Json.Decode as D


{-| Detect files dropped on an element.

This event handler will probably not work correctly unless you also add an
`onDragOver` handler with a no-op message. This is because you need to
preventDefault and stopPropagation on that event, or you will get the default
behavior for dropping files on a webpage. Currently, Elm doesn't have a way to
do that without also sending a message.

-}
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


{-| Receive messages when something is being dragged over the element.

This handler cancels the default behavior and event propagation for the event.
This is necessary for drop events to be handled correctly. You probably don't
want to do anything with the message this sends, and it will send **a lot** of
them.

-}
onDragOver : msg -> Attribute msg
onDragOver msg =
    custom "dragover" <| D.succeed { message = msg, stopPropagation = True, preventDefault = True }


{-| Detect when a dragged element enters the drop zone of this element.

You can use this to change the appearance of the element in some way to
indicate that dropping the element would be valid and do something interesting.

-}
onDragEnter : msg -> Attribute msg
onDragEnter msg =
    custom "dragenter" <| D.succeed { message = msg, stopPropagation = True, preventDefault = True }


{-| Detect when a dragged element leaves the drop zone of this element.

You can use this to reverse any changes you made when the drag entered the drop
zone.

-}
onDragLeave : msg -> Attribute msg
onDragLeave msg =
    custom "dragleave" <| D.succeed { message = msg, stopPropagation = True, preventDefault = True }
