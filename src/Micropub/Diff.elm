module Micropub.Diff exposing
    ( Deletions(..)
    , Diff
    , diff
    , encode
    , hasChanges
    )

import Dict
import Json.Decode as D
import Json.Encode as E
import Microformats as MF


type alias Diff =
    { url : String
    , add : MF.PropertyDict
    , replace : MF.PropertyDict
    , delete : Deletions
    }


type Deletions
    = DeleteProps (List String)
    | DeleteValues MF.PropertyDict


diff : MF.Item -> MF.Item -> Diff
diff original edited =
    let
        adds =
            Dict.empty

        deletes =
            Dict.diff original.properties edited.properties
                |> Dict.keys
                |> DeleteProps

        replaces =
            Dict.filter
                (\prop vals ->
                    case Dict.get prop original.properties of
                        Nothing ->
                            True

                        Just o ->
                            vals /= o
                )
                edited.properties
    in
    { url = Maybe.withDefault "" (MF.string "url" original)
    , add = adds
    , replace = replaces
    , delete = deletes
    }


hasChanges : Diff -> Bool
hasChanges d =
    hasAddChanges d || hasReplaceChanges d || hasDeleteChanges d


hasAddChanges : Diff -> Bool
hasAddChanges d =
    not (Dict.isEmpty d.add)


hasReplaceChanges : Diff -> Bool
hasReplaceChanges d =
    not (Dict.isEmpty d.replace)


hasDeleteChanges : Diff -> Bool
hasDeleteChanges d =
    case d.delete of
        DeleteProps ps ->
            not (List.isEmpty ps)

        DeleteValues vs ->
            not (Dict.isEmpty vs)


encode : Diff -> E.Value
encode d =
    E.object
        ([ ( "action", E.string "update" )
         , ( "url", E.string d.url )
         ]
            ++ addChanges d
            ++ replaceChanges d
            ++ deleteChanges d
        )


addChanges : Diff -> List ( String, E.Value )
addChanges d =
    if hasAddChanges d then
        [ ( "add", MF.encodeProperties d.add ) ]

    else
        []


replaceChanges : Diff -> List ( String, E.Value )
replaceChanges d =
    if hasReplaceChanges d then
        [ ( "replace", MF.encodeProperties d.replace ) ]

    else
        []


deleteChanges : Diff -> List ( String, E.Value )
deleteChanges d =
    if hasDeleteChanges d then
        [ ( "delete"
          , case d.delete of
                DeleteProps props ->
                    E.list E.string props

                DeleteValues dict ->
                    MF.encodeProperties dict
          )
        ]

    else
        []
