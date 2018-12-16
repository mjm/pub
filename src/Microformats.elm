module Microformats exposing (Item, Parsed, PropertyValue(..), decoder, feedEntries, getLink)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)


type alias Parsed =
    { items : List Item
    , rels : Dict String (List String)
    }


type alias Item =
    { type_ : String
    , properties : Dict String (List PropertyValue)
    , children : Children
    }


type Children
    = Children (List Item)


type PropertyValue
    = Str String
    | Embedded String String
    | Child Item


decoder : D.Decoder Parsed
decoder =
    D.succeed Parsed
        |> required "items" (D.list itemDecoder)
        |> required "rels" relsDecoder


itemDecoder : D.Decoder Item
itemDecoder =
    D.succeed Item
        |> required "type" (D.index 0 D.string)
        |> required "properties" propertiesDecoder
        |> optional "children" childrenDecoder (Children [])


propertiesDecoder : D.Decoder (Dict String (List PropertyValue))
propertiesDecoder =
    D.dict (D.list propertyDecoder)


propertyDecoder : D.Decoder PropertyValue
propertyDecoder =
    D.oneOf
        [ D.map Str D.string
        , D.map2 Embedded (D.field "value" D.string) (D.field "html" D.string)
        , D.map Child (D.lazy (\_ -> itemDecoder))
        ]


childrenDecoder : D.Decoder Children
childrenDecoder =
    D.map Children <| D.lazy (\_ -> D.list itemDecoder)


relsDecoder : D.Decoder (Dict String (List String))
relsDecoder =
    D.dict (D.list D.string)


getLink : String -> Parsed -> Maybe String
getLink rel parsed =
    case Dict.get rel parsed.rels of
        Just (x :: _) ->
            Just x

        _ ->
            Nothing


feedEntries : Parsed -> List Item
feedEntries parsed =
    parsed.items
        |> List.filter (\x -> x.type_ == "h-feed")
        |> List.concatMap
            (\x ->
                case x.children of
                    Children items ->
                        items
            )
