module Urls exposing
    ( editPage
    , editPost
    , home
    , newPost
    )

import Micropub.PostType as PostType
import Url.Builder as UB


home : String
home =
    "/"


editPost : String -> String
editPost url =
    "/posts/edit" ++ UB.toQuery [ UB.string "url" url ]


newPost : PostType.PostType -> String
newPost pt =
    "/posts/new" ++ UB.toQuery [ UB.string "type" (PostType.key pt) ]


editPage : String -> String
editPage path =
    "/pages/edit" ++ UB.toQuery [ UB.string "path" path ]
