module MicroformatsTests exposing (suite)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Microformats as MF
import Test exposing (..)


fuzzEntry : Fuzzer MF.PropertyValue -> Fuzzer MF.Item
fuzzEntry fuzzProp =
    let
        entry =
            MF.createEntry
    in
    Fuzz.map
        (\props ->
            { entry | properties = props }
        )
        (fuzzProperties fuzzProp)


fuzzProperties : Fuzzer MF.PropertyValue -> Fuzzer MF.PropertyDict
fuzzProperties fuzzProp =
    Fuzz.tuple ( Fuzz.string, fuzzProperty fuzzProp )
        |> Fuzz.list
        |> Fuzz.map Dict.fromList


fuzzProperty : Fuzzer MF.PropertyValue -> Fuzzer (List MF.PropertyValue)
fuzzProperty fuzzProp =
    Fuzz.map2 List.append
        (Fuzz.map List.singleton fuzzProp)
        (Fuzz.list fuzzProp)


fuzzPropertyValue : Fuzzer MF.PropertyValue
fuzzPropertyValue =
    Fuzz.oneOf
        [ fuzzStringValue ]


fuzzStringValue : Fuzzer MF.PropertyValue
fuzzStringValue =
    Fuzz.map MF.Str Fuzz.string


fuzzEmbeddedValue : Fuzzer MF.PropertyValue
fuzzEmbeddedValue =
    Fuzz.map2 MF.Embedded Fuzz.string Fuzz.string


suite : Test
suite =
    describe "Microformats"
        [ describe "getting a string property"
            [ fuzz (fuzzEntry fuzzStringValue) "returns the first value if they are strings" <|
                \entry ->
                    let
                        key =
                            List.head (Dict.keys entry.properties)
                    in
                    case key of
                        Just k ->
                            MF.string k entry
                                |> Expect.notEqual Nothing

                        Nothing ->
                            MF.string "foo" entry
                                |> Expect.equal Nothing
            , fuzz (fuzzEntry fuzzEmbeddedValue) "returns nothing if they are not strings" <|
                \entry ->
                    let
                        key =
                            List.head (Dict.keys entry.properties)
                    in
                    case key of
                        Just k ->
                            MF.string k entry
                                |> Expect.equal Nothing

                        Nothing ->
                            MF.string "foo" entry
                                |> Expect.equal Nothing
            ]
        ]
