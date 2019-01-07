module Http.Util exposing (expectCreated)

import Dict
import Http


{-| Expect a 201 Created response from an HTTP request.

Returns the value of the `Location` header from the response.

-}
expectCreated : (Result Http.Error String -> msg) -> Http.Expect msg
expectCreated toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case metadata.statusCode of
                        201 ->
                            case Dict.get "location" metadata.headers of
                                Just url ->
                                    Ok url

                                Nothing ->
                                    Err (Http.BadBody "No Location header found")

                        _ ->
                            Err (Http.BadStatus metadata.statusCode)
