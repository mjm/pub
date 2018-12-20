module Micropub.Media exposing (upload)

import File exposing (File)
import Http
import IndieAuth as Auth
import Micropub as MP


upload : (Result Http.Error String -> msg) -> File -> MP.Session -> Cmd msg
upload toMsg file session =
    case session.mediaUrl of
        Nothing ->
            Cmd.none

        Just url ->
            Http.request
                { method = "POST"
                , headers = [ Auth.header session.token ]
                , url = url
                , body =
                    Http.multipartBody
                        [ Http.filePart "file" file ]
                , expect = MP.expectCreated toMsg
                , timeout = Nothing
                , tracker = Nothing
                }
