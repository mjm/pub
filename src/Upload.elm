module Upload exposing
    ( FileBag
    , Tag
    , Upload(..)
    , append
    , displayUrls
    , emptyBag
    , fromUrls
    , setDataUrl
    , setUploadedUrl
    , urls
    )

import Array exposing (Array)
import File exposing (File)
import Http
import Micropub as MP
import Micropub.Media as Media
import Task


type alias FileBag =
    Array Upload


type Upload
    = ExistingFile String
    | NewFile { file : File, dataUrl : Maybe String, url : Maybe String }


type Tag
    = Tag Int


emptyBag : FileBag
emptyBag =
    Array.empty


fromUrls : List String -> FileBag
fromUrls us =
    Array.fromList (List.map ExistingFile us)


urls : FileBag -> List String
urls bag =
    List.filterMap uploadUrl (Array.toList bag)


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
