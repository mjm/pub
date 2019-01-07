module Page.Home exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import IndieAuth as Auth
import Microformats
import Micropub as MP
import Micropub.Html as MPH
import Micropub.PostType as PostType exposing (PostType(..))
import Session
import Skeleton
import Urls


type alias Model =
    { session : Session.LoggedInData
    }


init : Session.LoggedInData -> ( Model, Cmd Message )
init session =
    ( { session = session }
    , Cmd.none
    )


type Message
    = NoOp


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Skeleton.Details Message
view model =
    { title = "Welcome"
    , body =
        [ p [] [ text "This blog supports the following post types:" ]
        , ul [ class "list-reset flex mt-4" ]
            (List.map
                (\t ->
                    li []
                        [ a
                            [ class "text-sm font-bold bg-blue-dark text-white px-3 py-2 mx-2 rounded no-underline"
                            , href (Urls.newPost t)
                            ]
                            [ i [ class ("mr-2 fas fa-" ++ postTypeIcon t) ] []
                            , text (PostType.name t)
                            ]
                        ]
                )
                (MP.postTypes model.session.config)
            )
        ]
    , session = model.session
    , selection = Skeleton.Empty
    }


postTypeIcon : PostType -> String
postTypeIcon t =
    case t of
        Note _ ->
            "comment-alt"

        Article _ ->
            "paragraph"

        Photo _ ->
            "camera"

        Unknown _ _ ->
            ""
