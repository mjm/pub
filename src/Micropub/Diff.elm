module Micropub.Diff exposing
    ( Diff, Deletions(..)
    , diff, hasChanges, encode
    )

{-| This module provides a way to represent the difference in the properties
of two microformats2 items.

This representation can be used to update posts on a Micropub server.


# Definitions

@docs Diff, Deletions


# Using Diffs

@docs diff, hasChanges, encode

-}

import Dict
import Json.Decode as D
import Json.Encode as E
import Microformats as MF


{-| A Diff is a description of changes to make to the properties of an item.

There are three different types of changes:

  - `replace` indicates all values for a property are replaced with a new list
    of values.
  - `add` indicates that a new list of values are appended to the end of the
    current values for a property.
  - `delete` can take two different forms. It can either indicate a list of
    property names that should be removed from the item, or it can provide a
    list of values to be removed from the list of values for a property.

-}
type alias Diff =
    { url : String
    , add : MF.PropertyDict
    , replace : MF.PropertyDict
    , delete : Deletions
    }


{-| This type represents the list of deletions for a Diff.

Micropub supports both property deletions and value deletions, but only one of
those can be used in a single update request. This type captures those
semantics.

-}
type Deletions
    = DeleteProps (List String)
    | DeleteValues MF.PropertyDict


{-| Generate a diff of changes between one microformats item and another.

    diff oldPost newPost

This can be used to track what changes a user has made to a post by storing a
copy of the post before they make any changes, then creating a diff between
that post and the one that contains their changes. The resulting diff can then
be used to apply those changes on the server if desired.

-}
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


{-| Checks if a diff actually contains any changes.
-}
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


{-| Encode a diff as JSON.

The JSON representation of the diff corresponds to the expected Micropub
request body for an update to a post.

-}
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
