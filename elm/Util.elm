module Util exposing (customDecoder, listNth, targetValueIntParse)

import Char
import Html.Events exposing (on, onInput, targetValue)
import Json.Decode as Json


listNth n xs =
    List.head (List.drop n xs)


customDecoder : Json.Decoder a -> (a -> Result String b) -> Json.Decoder b
customDecoder d f =
    let
        resultDecoder x =
            case x of
                Ok a ->
                    Json.succeed a

                Err e ->
                    Json.fail e
    in
    Json.map f d |> Json.andThen resultDecoder


targetValueIntParse : Json.Decoder Int
targetValueIntParse =
    targetValue
        |> Json.andThen
            (\str ->
                case String.toInt str of
                    Just i ->
                        Json.succeed i

                    Nothing ->
                        Json.fail "Error"
            )
