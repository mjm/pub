module View.PostType exposing (postTypeIcon)

import Micropub.PostType exposing (PostType(..))


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
