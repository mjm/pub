module View.Photos exposing
    ( Message
    , Model
    , init
    , setUrls
    , update
    , urls
    , view
    )

import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Drag exposing (..)
import Http
import Micropub
import Upload


type Model
    = Model
        { isDragging : Bool
        , files : Upload.FileBag
        , micropub : Micropub.Session
        }


init : Micropub.Session -> Model
init micropub =
    Model
        { isDragging = False
        , files = Upload.emptyBag
        , micropub = micropub
        }


urls : Model -> List String
urls (Model model) =
    Upload.urls model.files


setUrls : List String -> Model -> Model
setUrls us (Model model) =
    Model { model | files = Upload.fromUrls us }


type Message
    = NoOp
    | SetDragging Bool
    | AddFiles (List File)
    | GotDataUrl Upload.Tag String
    | UploadedFile Upload.Tag (Result Http.Error String)
    | DeleteFile Upload.Tag


update : Message -> Model -> ( Model, Cmd Message )
update msg (Model model) =
    let
        ( m, cmds ) =
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                SetDragging isDragging ->
                    ( { model | isDragging = isDragging }, Cmd.none )

                AddFiles files ->
                    let
                        ( newFiles, cmd ) =
                            Upload.append GotDataUrl UploadedFile model.micropub files model.files
                    in
                    ( { model | files = newFiles, isDragging = False }, cmd )

                GotDataUrl tag url ->
                    ( { model | files = Upload.setDataUrl tag url model.files }, Cmd.none )

                UploadedFile tag (Ok url) ->
                    ( { model | files = Upload.setUploadedUrl tag url model.files }, Cmd.none )

                UploadedFile _ (Err _) ->
                    ( model, Cmd.none )

                DeleteFile tag ->
                    ( { model | files = Upload.remove tag model.files }, Cmd.none )
    in
    ( Model m, cmds )


view :
    { attrs : List (Attribute msg)
    , toMsg : Message -> msg
    }
    -> Model
    -> Html msg
view config (Model model) =
    let
        us =
            Upload.displayUrls model.files
    in
    div
        [ class "mt-2 p-3 flex flex-col rounded-lg"
        , class
            (if model.isDragging then
                "bg-orange-lighter"

             else
                "bg-orange-lightest"
            )
        , class
            (if List.isEmpty us then
                "h-16"

             else
                "h-48"
            )
        , onDrop (config.toMsg << AddFiles)
        , onDragOver (config.toMsg NoOp)
        , onDragEnter ((config.toMsg << SetDragging) True)
        , onDragLeave ((config.toMsg << SetDragging) False)
        ]
        (if List.isEmpty us then
            [ p [ class "my-auto text-center pointer-events-none text-sm uppercase font-semibold text-orange-dark" ]
                [ i [ class "fas fa-images mr-1" ] []
                , text "Drop photos here"
                ]
            ]

         else
            [ h3 [ class "uppercase text-sm mb-2 text-orange-dark pointer-events-none" ]
                [ i [ class "fas fa-images mr-1" ] []
                , text <| "Photos (" ++ String.fromInt (List.length us) ++ ")"
                ]
            , p [ class "flex flex-row items-start overflow-hidden pointer-events-none" ] <|
                Upload.taggedMap
                    (\tag u ->
                        div [ class "w-1/4 max-h-full pointer-events-auto relative" ]
                            [ button
                                [ class "font-bold rounded-full text-sm bg-blue-dark text-white absolute pin-t pin-r m-2 w-6 h-6"
                                , type_ "button"
                                , onClick ((config.toMsg << DeleteFile) tag)
                                ]
                                [ i [ class "fas fa-times" ] [] ]
                            , img [ src u ] []
                            ]
                    )
                    model.files
            ]
        )
