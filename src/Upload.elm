module Upload exposing
    ( FileBag, Upload(..), Tag
    , emptyBag, fromUrls
    , urls, displayUrls
    , append, setDataUrl, setUploadedUrl
    )

{-| The Upload module provides a data structure for keeping track of files
that are being uploaded to the Micropub media endpoint.


# Definition

@docs FileBag, Upload, Tag


# Creating

@docs emptyBag, fromUrls


# Getting URLs

@docs urls, displayUrls


# Changing

@docs append, setDataUrl, setUploadedUrl

-}

import Array exposing (Array)
import File exposing (File)
import Http
import Micropub as MP
import Micropub.Media as Media
import Task


{-| A FileBag contains an ordered list of uploaded files.
-}
type alias FileBag =
    Array Upload


{-| An Upload is a single file in a FileBag.

An Upload can either be an existing file that was uploaded in a previous
session or a new file that the user just dragged in to the app. A new file may
or may not be uploaded to the server yet.

-}
type Upload
    = ExistingFile String
    | NewFile { file : File, dataUrl : Maybe String, url : Maybe String }


{-| A Tag is an opaque identifier for a particular file in a FileBag. It is
passed to messages regarding specific files and can be used to refer to those
files later when updating the bag.
-}
type Tag
    = Tag Int


{-| Creates a FileBag with no files in it.
-}
emptyBag : FileBag
emptyBag =
    Array.empty


{-| Creates a FileBag with some existing files that were previously uploaded
to the server.
-}
fromUrls : List String -> FileBag
fromUrls us =
    Array.fromList (List.map ExistingFile us)


{-| Gives a list of the URLs for all files that have been successfully uploaded
to the server.

Files that are still uploading or failed to upload are left out of the list.
Existing files that were uploaded in a previous session are included in the
list.

-}
urls : FileBag -> List String
urls bag =
    List.filterMap uploadUrl (Array.toList bag)


{-| Gives a list of displayable URLs for files in the bag.

Unlike urls, this list includes files that have not been uploaded to the server
yet, as long as a data URL has been generated and set for the file.

This can be used to display a list of the files the user has chosen to attach
without waiting for the file upload to complete.

-}
displayUrls : FileBag -> List String
displayUrls bag =
    List.filterMap displayUrl (Array.toList bag)


uploadUrl : Upload -> Maybe String
uploadUrl upload =
    case upload of
        ExistingFile url ->
            Just url

        NewFile file ->
            file.url


displayUrl : Upload -> Maybe String
displayUrl upload =
    case upload of
        ExistingFile url ->
            Just url

        NewFile file ->
            case ( file.url, file.dataUrl ) of
                ( Just u, _ ) ->
                    Just u

                ( Nothing, Just u ) ->
                    Just u

                _ ->
                    Nothing


{-| Add new files to the end of the bag, and start uploading their contents to
the server.

For each new file to upload, two commands will also be generated. The first
will convert the file to a data URL, which can be used to show a preview of the
file before uploading is complete. Use `setDataUrl` to store the resulting URL
back in the FileBag.

The second command will actually upload the file to the server. If the upload
succeeds, the message will be passed the remote URL of the uploaded file. Use
`setUploadedUrl` to store that URL back in the FileBag.

-}
append : (Tag -> String -> msg) -> (Tag -> Result Http.Error String -> msg) -> MP.Session -> List File -> FileBag -> ( FileBag, Cmd msg )
append dataUrlMsg uploadMsg session files bag =
    let
        startIndex =
            Array.length bag

        newFiles =
            Array.map newUpload (Array.fromList files)

        newBag =
            Array.append bag newFiles
    in
    ( newBag
    , Cmd.batch
        (Array.toList
            (Array.indexedMap
                (\i file ->
                    let
                        tag =
                            Tag (startIndex + i)
                    in
                    case file of
                        NewFile nf ->
                            Cmd.batch
                                [ Task.perform (dataUrlMsg tag) (File.toUrl nf.file)
                                , Media.upload (uploadMsg tag) nf.file session
                                ]

                        ExistingFile _ ->
                            Cmd.none
                )
                newFiles
            )
        )
    )


newUpload : File -> Upload
newUpload file =
    NewFile { file = file, dataUrl = Nothing, url = Nothing }


{-| Sets the data URL for a file in the bag.

A `data:` scheme URL encodes binary data inline in the URL. This can be used as
the `src` of an `img` tag to display an image whose contents only exist locally.
We use this to allow displaying images before they are done uploading.

-}
setDataUrl : Tag -> String -> FileBag -> FileBag
setDataUrl (Tag i) url bag =
    Array.indexedMap
        (\j file ->
            case file of
                ExistingFile _ ->
                    file

                NewFile nf ->
                    if i == j then
                        NewFile { nf | dataUrl = Just url }

                    else
                        file
        )
        bag


{-| Sets the remote URL for a file in the bag.

This is the URL where the file can be found on and fetched from the server. The
media endpoint for Micropub returns this URL in the `Location` header of a
successful upload request.

-}
setUploadedUrl : Tag -> String -> FileBag -> FileBag
setUploadedUrl (Tag i) url bag =
    Array.indexedMap
        (\j file ->
            case file of
                ExistingFile _ ->
                    file

                NewFile nf ->
                    if i == j then
                        NewFile { nf | url = Just url }

                    else
                        file
        )
        bag
