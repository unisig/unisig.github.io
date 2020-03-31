module Unisig exposing (fromNameBytes, signatureBytes, validateNameBytes)

import Char
import String
import Util


signatureBytes =
    [ 0xDC, 0xDC, 0x0D, 0x0A, 0x1A, 0x0A ]


validateNameBytes : List Int -> Result String (List Int)
validateNameBytes nameBytes =
    let
        nameString =
            String.join "" (List.map (Char.fromCode >> String.fromChar) nameBytes)
    in
    if List.length nameBytes < 1 then
        Err "Name cannot be blank"

    else if List.length nameBytes > 127 then
        Err "Name cannot be longer than 127 bytes"

    else if List.member (Char.toCode ' ') nameBytes then
        Err "Please use dashes instead of spaces"

    else if String.contains ".." nameString then
        Err "Please don't use multiple dots"

    else if nameString /= String.toLower nameString then
        Err "Please use lowercase letters only"

    else
        Ok nameBytes


fromNameBytes nameBytes =
    nameBytes
        |> validateNameBytes
        |> Result.map
            (\nameBytes2 -> signatureBytes ++ [ List.length nameBytes2 ] ++ nameBytes2)
