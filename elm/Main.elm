module Main exposing (main)

import Browser
import Bytes
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, targetValue)
import Json.Decode as Json
import Languages
import Maybe
import String
import Unisig
import Util


type Msg
    = SetName String
    | SetLanguage Int
    | SetAlignment Int


type alias Model =
    { name : String, languageIndex : Int, alignment : Int }


init : Model
init =
    { name = "github.com/example/format#2020-04-01"
    , languageIndex = 0
    , alignment = 1
    }


view : Model -> Html.Html Msg
view model =
    div []
        (List.append
            [ h1 [] [ text "Unisig maker" ]
            , p [] [ text "Generate a uniform signature for your file format or network protocol. " ]
            , input
                [ value model.name
                , size 50
                , onInput SetName
                ]
                []
            , select [ on "change" (Json.map SetAlignment Util.targetValueIntParse) ]
                (List.map
                    (\( intval, title ) -> option [ value (String.fromInt intval) ] [ text title ])
                    (( 1, "No alignment needed" )
                        :: List.map (\n -> ( n, String.fromInt n ++ "-byte alignment" )) [ 2, 4, 8 ]
                    )
                )
            ]
            (case Languages.code model.name model.languageIndex model.alignment of
                Err theErr ->
                    [ div [] [ text theErr ] ]

                Ok ( theBytes, theCode ) ->
                    [ h2 [] [ text "Hex bytes" ]
                    , pre [] [ text (theBytes |> Bytes.spaceSeparatedHexDump 0) ]
                    , h2 [] [ text "Source code" ]
                    , select [ on "change" (Json.map SetLanguage Util.targetValueIntParse) ]
                        (Languages.map
                            (\index name -> option [ value (String.fromInt index) ] [ text name ])
                        )
                    , pre [] [ text theCode ]
                    ]
            )
        )


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetName name ->
            { model | name = name }

        SetLanguage languageIndex ->
            { model | languageIndex = languageIndex }

        SetAlignment alignment ->
            { model | alignment = alignment }


main =
    Browser.sandbox { init = init, view = view, update = update }
