module Microformats exposing
    ( Item
    , Parsed
    , PropertyValue(..)
    , decoder
    , encodeItem
    , feedEntries
    , getLink
    , itemDecoder
    , setString
    , string
    )

import Dict exposing (Dict)
import ElmEscapeHtml exposing (unescape)
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E


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
        [ D.map Str escapedString
        , D.map2 Embedded (D.field "value" escapedString) (D.field "html" escapedString)
        , D.map Child (D.lazy (\_ -> itemDecoder))
        ]


childrenDecoder : D.Decoder Children
childrenDecoder =
    D.map Children <| D.lazy (\_ -> D.list itemDecoder)


relsDecoder : D.Decoder (Dict String (List String))
relsDecoder =
    D.dict (D.list D.string)


encodeItem : Item -> E.Value
encodeItem item =
    let
        children =
            case item.children of
                Children x ->
                    x
    in
    E.object
        [ ( "type", E.list E.string [ item.type_ ] )
        , ( "properties", E.dict identity (E.list encodeProperty) item.properties )
        , ( "children", E.list encodeItem children )
        ]


encodeProperty : PropertyValue -> E.Value
encodeProperty val =
    case val of
        Str s ->
            E.string s

        Embedded value html ->
            E.object
                [ ( "value", E.string value )
                , ( "html", E.string html )
                ]

        Child item ->
            encodeItem item


escapedString : D.Decoder String
escapedString =
    D.map unescape D.string


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


string : String -> Item -> Maybe String
string prop item =
    case Dict.get prop item.properties of
        Just ((Str x) :: _) ->
            Just x

        _ ->
            Nothing


setString : String -> String -> Item -> Item
setString prop val item =
    if String.isEmpty val then
        { item | properties = Dict.remove prop item.properties }

    else
        { item | properties = Dict.insert prop [ Str val ] item.properties }
