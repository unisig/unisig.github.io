module Bytes exposing (align, backslashEscapedString, fromString, hexByte, hexOrAsciiByte, isSafeAscii, lispByteVector, packLine, packLines, packLinesX, padding, spaceSeparatedHexDump)

import Char
import Util


isSafeAscii char =
    Char.isUpper char
        || Char.isLower char
        || Char.isDigit char
        || String.contains (String.fromChar char) ".:@-/#"


hexByte hexPrefix byte =
    let
        digits =
            String.toList "0123456789abcdef"

        hexDigit n =
            Util.listNth n digits |> Maybe.withDefault '0' |> String.fromChar
    in
    hexPrefix ++ hexDigit (byte // 16) ++ hexDigit (modBy 16 byte)


hexOrAsciiByte hexPrefix byte =
    let
        char =
            Char.fromCode byte
    in
    if isSafeAscii char then
        String.fromChar char

    else
        hexByte hexPrefix byte


packLine maxlen between items line =
    case items of
        [] ->
            ( line, items )

        item :: moreItems ->
            let
                lineItem =
                    line ++ between ++ item
            in
            if String.length lineItem > maxlen then
                ( line, items )

            else
                packLine maxlen between moreItems lineItem


packLines maxlen between items lines =
    case items of
        [] ->
            lines

        item :: moreItems ->
            let
                ( line, evenMoreItems ) =
                    packLine maxlen between moreItems item
            in
            packLines maxlen between evenMoreItems (List.append lines [ line ])


packLinesX indent prefix suffix between items =
    let
        indentPrefix =
            String.repeat indent " " ++ prefix

        maxlen =
            72 - String.length indentPrefix - String.length suffix

        lines =
            packLines maxlen between items []
    in
    lines |> List.map (\line -> indentPrefix ++ line ++ suffix) |> String.join "\n"


backslashEscapedString indent prefix suffix bytes =
    bytes |> List.map (hexOrAsciiByte "\\x") |> packLinesX indent prefix suffix ""


lispByteVector indent bytes =
    bytes |> List.map (hexByte "#x") |> packLinesX indent "" "" " "


spaceSeparatedHexDump indent bytes =
    bytes |> List.map (hexByte "") |> packLinesX indent "" "" " "


fromString : String -> List Int
fromString string =
    string |> String.toList |> List.map Char.toCode


padding : Int -> Int -> Int
padding alignment length =
    modBy alignment (alignment - modBy alignment length)


align alignment bytes =
    List.append bytes (List.repeat (List.length bytes |> padding alignment) 0)
