module Languages exposing (Language, cStdio, code, commonLisp, defaultLanguage, goLang, languageByIndex, languages, map, python3)

import Bytes
import Char
import Unisig
import Util


type alias Language =
    ( String, List Int -> List String )


cStdio =
    ( "C (stdio)"
    , \bytes ->
        [ "#include <stdio.h>"
        , "#include <string.h>"
        , ""
        , "static const unsigned char unisig[" ++ String.fromInt (List.length bytes) ++ "] ="
        , Bytes.backslashEscapedString 4 "\"" "\"" bytes ++ ";"
        , ""
        , "static void write_unisig(FILE *stream)"
        , "{"
        , "    if (1 != fwrite(unisig, sizeof(unisig), 1, stream)) {"
        , "        die();"
        , "    }"
        , "}"
        , ""
        , "static void read_unisig(FILE *stream)"
        , "{"
        , "    unsigned char buf[sizeof(unisig)];"
        , ""
        , "    if (1 != fread(buf, sizeof(unisig), 1, stream)) {"
        , "        die();"
        , "    }"
        , "    if (memcmp(buf, unisig, sizeof(unisig))) {"
        , "        die();"
        , "    }"
        , "}"
        ]
    )


commonLisp =
    ( "Common Lisp"
    , \bytes ->
        [ "(defconstant +unisig+"
        , "  (make-array "
            ++ String.fromInt (List.length bytes)
            ++ " :element-type '(unsigned-byte 8)"
        , "   :initial-contents '("
        , Bytes.lispByteVector 4 bytes
            ++ ")))"
        , ""
        , "(defun write-unisig (stream)"
        , "  (write-sequence +unisig+ stream))"
        , ""
        , "(defun read-unisig (stream)"
        , "  (let ((buf (make-array (length +unisig+)"
        , "               :element-type '(unsigned-byte 8) :initial-element 0)))"
        , "    (unless (and (= (length +unisig+) (read-sequence buf stream))"
        , "                 (equal +unisig+ buf))"
        , "      (error \"Unknown file format\"))))"
        ]
    )


goLang =
    ( "Go"
    , \bytes ->
        [ "import \"os\""
        , ""
        , "const unisig = "
        , Bytes.backslashEscapedString 4 "\"" "\"" bytes
        , ""
        , "func writeUnisig() {"
        , "    os.Stdout.WriteString(unisig)"
        , "}"
        , ""
        , "func readUnisig() {"
        , "    var buf [len(unisig)]byte"
        , "    n, err := os.Stdin.Read(buf)"
        , "    if err != nil {"
        , "        panic(err)"
        , "    }"
        , "    if n != len(unisig) {"
        , "        panic(\"Unknown file format\")"
        , "    }"
        , "    if !bytes.Equal(buf, unisig) {"
        , "        panic(\"Unknown file format\")"
        , "    }"
        , "}"
        ]
    )


python3 =
    ( "Python 3"
    , \bytes ->
        [ "UNISIG = \\"
        , Bytes.backslashEscapedString 4 "b\"" "\" \\" bytes
        , ""
        , "def write_unisig():"
        , "    sys.stdout.buffer.write(UNISIG)"
        , ""
        , "def read_unisig():"
        , "    if sys.stdin.buffer.read(len(UNISIG)) != UNISIG:"
        , "        raise IOError('Unknown file format')"
        ]
    )


schemeR6RS =
    ( "Scheme (R6RS)"
    , \bytes ->
        [ "(define unisig"
        , "  #vu8("
        , Bytes.lispByteVector 7 bytes
            ++ "))"
        , ""
        , "(define write-unisig (port) (put-bytevector port unisig))"
        , ""
        , "(define read-unisig (port)"
        , "  (let ((buf (get-bytevector-n port (bytevector-length unisig))))"
        , "    (unless (equal? buf unisig)"
        , "      (error #f \"Unknown file format\"))))"
        ]
    )


schemeR7RS =
    ( "Scheme (R7RS)"
    , \bytes ->
        [ "(define unisig"
        , "  #u8("
        , Bytes.lispByteVector 6 bytes
            ++ "))"
        , ""
        , "(define write-unisig (port) (write-bytevector unisig port))"
        , ""
        , "(define read-unisig (port)"
        , "  (let ((buf (read-bytevector port (bytevector-length unisig))))"
        , "    (unless (equal? buf unisig)"
        , "      (error \"Unknown file format\"))))"
        ]
    )


languages : List Language
languages =
    [ cStdio
    , commonLisp
    , goLang
    , python3
    , schemeR6RS
    , schemeR7RS
    ]


defaultLanguage =
    cStdio


languageByIndex languageIndex =
    Util.listNth languageIndex languages |> Maybe.withDefault defaultLanguage


map : (Int -> String -> a) -> List a
map f =
    List.indexedMap (\index ( name, _ ) -> f index name) languages


code : String -> Int -> Int -> Result String ( List Int, String )
code nameString languageIndex alignment =
    let
        ( languageName, languageCodeLines ) =
            languageByIndex languageIndex
    in
    nameString
        |> Bytes.fromString
        |> Unisig.fromNameBytes
        |> Result.map
            (\unalignedBytes ->
                let
                    bytes =
                        Bytes.align alignment unalignedBytes
                in
                ( bytes, bytes |> languageCodeLines |> String.join "\n" )
            )
