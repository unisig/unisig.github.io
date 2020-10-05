module Bytes exposing
    ( ascify
    , fromString
    , hexByte
    , hexOrAsciiByte
    , hexify
    , isSafeAscii
    , padBytes
    , padLines
    , spaceSeparatedHexDump
    )

import Char
import Util


isSafeAscii char =
    Char.isUpper char
        || Char.isLower char
        || Char.isDigit char
        || String.contains (String.fromChar char) ".:@-/#"


hexByte hexPrefix hexSuffix byte =
    let
        digits =
            String.toList "0123456789abcdef"

        hexDigit n =
            Util.listNth n digits |> Maybe.withDefault '0' |> String.fromChar
    in
    hexPrefix
        ++ hexDigit (byte // 16)
        ++ hexDigit (modBy 16 byte)
        ++ hexSuffix


hexOrAsciiByte hexPrefix hexSuffix byte =
    let
        char =
            Char.fromCode byte
    in
    if isSafeAscii char then
        String.fromChar char

    else
        hexByte hexPrefix hexSuffix byte


hexify hexPrefix hexSuffix between bytes =
    stringsToLines between
        (List.map (hexByte hexPrefix hexSuffix)
            bytes
        )


ascify hexPrefix hexSuffix between bytes =
    stringsToLines between
        (List.map (hexOrAsciiByte hexPrefix hexSuffix)
            bytes
        )


stringsToLines between strings =
    let
        maxLineLength =
            60

        loop moreStrings lines line =
            case moreStrings of
                [] ->
                    List.reverse
                        (if line == "" then
                            lines

                         else
                            line :: lines
                        )

                string :: evenMoreStrings ->
                    let
                        extendedLine =
                            line
                                ++ (if line == "" then
                                        ""

                                    else
                                        between
                                   )
                                ++ string
                    in
                    if String.length extendedLine > maxLineLength then
                        loop evenMoreStrings (line :: lines) string

                    else
                        loop evenMoreStrings lines extendedLine
    in
    loop strings [] ""


padLines firstLinePrefix nonfirstLinePrefix nonlastLineSuffix lastLineSuffix lines =
    let
        loop moreLines isFirst paddedLines =
            case moreLines of
                [] ->
                    List.reverse paddedLines

                line :: evenMoreLines ->
                    let
                        isLast =
                            evenMoreLines == []

                        paddedLine =
                            (if isFirst then
                                firstLinePrefix

                             else
                                nonfirstLinePrefix
                            )
                                ++ line
                                ++ (if isLast then
                                        lastLineSuffix

                                    else
                                        nonlastLineSuffix
                                   )
                    in
                    loop evenMoreLines False (paddedLine :: paddedLines)
    in
    loop lines True []


spaceSeparatedHexDump bytes =
    String.join "\n" (hexify "" "" " " bytes)


fromString : String -> List Int
fromString string =
    string |> String.toList |> List.map Char.toCode


padBytes alignment length =
    List.repeat (modBy alignment (alignment - modBy alignment length))
        0
