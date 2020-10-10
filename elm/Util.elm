module Util exposing (customDecoder, formatIsoDate, listNth, targetValueIntParse)

import Char
import Html.Events exposing (on, onInput, targetValue)
import Json.Decode as Json
import Time


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


formatIsoDate zone t =
    let
        monthNumber month =
            case month of
                Time.Jan ->
                    1

                Time.Feb ->
                    2

                Time.Mar ->
                    3

                Time.Apr ->
                    4

                Time.May ->
                    5

                Time.Jun ->
                    6

                Time.Jul ->
                    7

                Time.Aug ->
                    8

                Time.Sep ->
                    9

                Time.Oct ->
                    10

                Time.Nov ->
                    11

                Time.Dec ->
                    12

        y =
            String.fromInt (Time.toYear zone t)

        m =
            String.fromInt (monthNumber (Time.toMonth zone t))

        d =
            String.fromInt (Time.toDay zone t)
    in
    y ++ "-" ++ String.pad 2 ' ' m ++ "-" ++ String.pad 2 ' ' d
