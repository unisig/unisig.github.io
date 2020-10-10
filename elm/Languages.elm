module Languages exposing
    ( Language
    , code
    , map
    )

import Bytes
import Char
import Unisig
import Util


type alias Language =
    ( String, ( List Int, List Int ) -> List String )


cStdio =
    ( "C (stdio)"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "#include <stdio.h>"
              , "#include <string.h>"
              , ""
              , "static const unsigned char unisig["
                    ++ String.fromInt
                        (List.length headBytes
                            + List.length bodyBytes
                        )
                    ++ "] ="
              ]
            , Bytes.padLines
                "    \""
                "    \""
                "\""
                "\";"
                (List.append
                    (Bytes.hexify "\\x" "" "" headBytes)
                    (Bytes.ascify "\\x" "" "" bodyBytes)
                )
            , [ ""
              , "static void write_unisig(FILE *stream)"
              , "{"
              , "    if (1 != fwrite(unisig, sizeof(unisig), 1, stream)) {"
              , "        exit(EXIT_FAILURE);"
              , "    }"
              , "}"
              , ""
              , "static void read_unisig(FILE *stream)"
              , "{"
              , "    unsigned char buf[sizeof(unisig)];"
              , ""
              , "    if (1 != fread(buf, sizeof(unisig), 1, stream)) {"
              , "        exit(EXIT_FAILURE);"
              , "    }"
              , "    if (memcmp(buf, unisig, sizeof(unisig))) {"
              , "        exit(EXIT_FAILURE);"
              , "    }"
              , "}"
              ]
            ]
    )


cPlusPlusIostream =
    ( "C++ (iostream)"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "#include <cstring>"
              , "#include <iostream>"
              , ""
              , "static const unsigned char unisig["
                    ++ String.fromInt
                        (List.length headBytes
                            + List.length bodyBytes
                        )
                    ++ "] = {"
              ]
            , Bytes.padLines
                "    "
                "    "
                ""
                ""
                (Bytes.hexify "0x" "," " " (List.append headBytes bodyBytes))
            , [ "};"
              , ""
              , "static void write_unisig(std::ostream &stream)"
              , "{"
              , "    stream.write(reinterpret_cast<const char *>(unisig), sizeof(unisig));"
              , "    if (!stream.good()) {"
              , "        exit(EXIT_FAILURE);"
              , "    }"
              , "}"
              , ""
              , "static void read_unisig(std::istream &stream)"
              , "{"
              , "    char buf[sizeof(unisig)];"
              , "    stream.read(buf, sizeof(unisig));"
              , "    if (!stream.good()) {"
              , "        exit(EXIT_FAILURE);"
              , "    }"
              , "    if (stream.gcount() != sizeof(unisig)) {"
              , "        exit(EXIT_FAILURE);"
              , "    }"
              , "    if (memcmp(buf, unisig, sizeof(unisig))) {"
              , "        exit(EXIT_FAILURE);"
              , "    }"
              , "}"
              ]
            ]
    )


commonLisp =
    ( "Common Lisp"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "(defconstant +unisig+"
              , "  (make-array"
              , "   "
                    ++ String.fromInt
                        (List.length headBytes
                            + List.length bodyBytes
                        )
              , "   :element-type '(unsigned-byte 8)"
              , "   :initial-contents"
              ]
            , Bytes.padLines
                "   '("
                "     "
                ""
                ")))"
                (Bytes.hexify "#x" "" " " (List.append headBytes bodyBytes))
            , [ ""
              , "(defun write-unisig (stream)"
              , "  (write-sequence +unisig+ stream))"
              , ""
              , "(defun read-unisig (stream)"
              , "  (let ((buf (make-array (length +unisig+)"
              , "                         :element-type '(unsigned-byte 8)"
              , "                         :initial-element 0)))"
              , "    (unless (and (= (length +unisig+)"
                    ++ " (read-sequence buf stream))"
              , "                 (equal +unisig+ buf))"
              , "      (error \"Unknown file format\"))))"
              ]
            ]
    )


goLang =
    ( "Go"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "import \"os\""
              , ""
              , "const unisig ="
              ]
            , Bytes.padLines
                "    \""
                "    \""
                "\" +"
                "\""
                (List.append
                    (Bytes.hexify "\\x" "" "" headBytes)
                    (Bytes.ascify "\\x" "" "" bodyBytes)
                )
            , [ ""
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
            ]
    )


python3 =
    ( "Python 3"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "UNISIG = \\" ]
            , Bytes.padLines
                "    b\""
                "    b\""
                "\" \\"
                "\""
                (List.append
                    (Bytes.hexify "\\x" "" "" headBytes)
                    (Bytes.ascify "\\x" "" "" bodyBytes)
                )
            , [ ""
              , "def write_unisig():"
              , "    sys.stdout.buffer.write(UNISIG)"
              , ""
              , "def read_unisig():"
              , "    if sys.stdin.buffer.read(len(UNISIG)) != UNISIG:"
              , "        raise IOError('Unknown file format')"
              ]
            ]
    )


ruby =
    ( "Ruby"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "UNISIG = \\" ]
            , Bytes.padLines
                "  \""
                "  \""
                "\" \\"
                "\" \\"
                (List.append
                    (Bytes.hexify "\\x" "" "" headBytes)
                    (Bytes.ascify "\\x" "" "" bodyBytes)
                )
            , [ "  .force_encoding('binary')"
              , ""
              , "def write_unisig(io)"
              , "  raise unless io.binmode?"
              , "  io.write(UNISIG)"
              , "end"
              , ""
              , "def read_unisig(io)"
              , "  raise unless io.binmode?"
              , "  if io.read(UNISIG.length) != UNISIG"
              , "    raise IOError.new('Unknown file format')"
              , "  end"
              , "end"
              ]
            ]
    )


schemeR6RS =
    ( "Scheme (R6RS)"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "(define unisig" ]
            , Bytes.padLines
                "  #vu8("
                "       "
                ""
                "))"
                (Bytes.hexify "#x" "" " " (List.append headBytes bodyBytes))
            , [ ""
              , "(define write-unisig (port) (put-bytevector port unisig))"
              , ""
              , "(define read-unisig (port)"
              , "  (let ((buf (get-bytevector-n port"
                    ++ " (bytevector-length unisig))))"
              , "    (unless (equal? buf unisig)"
              , "      (error #f \"Unknown file format\"))))"
              ]
            ]
    )


schemeR7RS =
    ( "Scheme (R7RS)"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "(define unisig" ]
            , Bytes.padLines
                "  #u8("
                "      "
                ""
                "))"
                (Bytes.hexify "#x" "" " " (List.append headBytes bodyBytes))
            , [ ""
              , "(define write-unisig (port) (write-bytevector unisig port))"
              , ""
              , "(define read-unisig (port)"
              , "  (let ((buf (read-bytevector port"
                    ++ " (bytevector-length unisig))))"
              , "    (unless (equal? buf unisig)"
              , "      (error \"Unknown file format\"))))"
              ]
            ]
    )


standardML =
    ( "Standard ML"
    , \( headBytes, bodyBytes ) ->
        List.concat
            [ [ "exception BadUnisig;"
              , ""
              , "val unisig ="
              , "    Word8Vector.fromList ["
              ]
            , Bytes.padLines
                "        "
                "        "
                ","
                ""
                (Bytes.hexify "0wx" "" ", " (List.append headBytes bodyBytes))
            , [ "    ];"
              , ""
              , "fun word8VectorEqual a b ="
              , "    let fun check 0 = true"
              , "          | check n = (Word8Vector.sub (a, (n - 1)) ="
              , "                       Word8Vector.sub (b, (n - 1)))"
              , "                      andalso check (n - 1)"
              , "        val an = Word8Vector.length a"
              , "        val bn = Word8Vector.length b"
              , "    in an = bn andalso check an end;"
              , ""
              , "fun writeUnisig ostream = BinIO.output (ostream, unisig);"
              , ""
              , "fun readUnisig istream ="
              , "    let val len = (Word8Vector.length unisig)"
              , "        val data = BinIO.inputN (istream, len)"
              , "    in if word8VectorEqual data unisig then () else raise BadUnisig end;"
              ]
            ]
    )


languages : List Language
languages =
    [ cStdio
    , cPlusPlusIostream
    , commonLisp
    , goLang
    , python3
    , ruby
    , schemeR6RS
    , schemeR7RS
    , standardML
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
        |> Unisig.headAndBodyFromNameBytes alignment
        |> Result.map
            (\( head, body ) ->
                ( List.append head body
                , String.join "\n" (languageCodeLines ( head, body ))
                )
            )
