module Micropub.Media exposing (upload)

{-| This module supports uploading files using a Micropub server's media
endpoint.


# Uploading

@docs upload

-}

import File exposing (File)
import Http
import Http.Util exposing (expectCreated)
import IndieAuth as Auth
import Micropub as MP


{-| Uploads a file to a Micropub server.

If the session does not have a media endpoint URL for the server, this produces
no command.

If the file is uploaded successfully, the result in the message will contain
the URL from which the uploaded file can be downloaded.

-}
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
                , expect = expectCreated toMsg
                , timeout = Nothing
                , tracker = Nothing
                }
