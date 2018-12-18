module Urls exposing
    ( editPage
    , editPost
    , home
    , newPost
    )

import Micropub
import Url.Builder as UB


home : String
home =
    "/"


editPost : String -> String
editPost url =
    "/posts/edit" ++ UB.toQuery [ UB.string "url" url ]


newPost : Micropub.PostType -> String
newPost pt =
    "/posts/new" ++ UB.toQuery [ UB.string "type" (Micropub.postTypeKey pt) ]


editPage : String -> String
editPage path =
    "/pages/edit" ++ UB.toQuery [ UB.string "path" path ]
