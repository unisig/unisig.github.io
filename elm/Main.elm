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
import Task
import Time
import Unisig
import Util


type Msg
    = Initialize Time.Posix
    | SetName String
    | SetLanguage Int
    | SetAlignment Int


type alias Model =
    { name : String, languageIndex : Int, alignment : Int }


initialName =
    "github.com/example/format"


initialNameWithTime t =
    initialName ++ "#" ++ Util.formatIsoDate Time.utc t


init () =
    ( { name = initialName
      , languageIndex = 0
      , alignment = 1
      }
    , Task.perform Initialize Time.now
    )


alignmentDropdown model =
    select [ on "change" (Json.map SetAlignment Util.targetValueIntParse) ]
        (List.map
            (\( intval, title ) ->
                option [ value (String.fromInt intval) ] [ text title ]
            )
            (( 1, "No alignment needed" )
                :: List.map
                    (\n -> ( n, String.fromInt n ++ "-byte alignment" ))
                    [ 2, 4, 8 ]
            )
        )


languageDropdown model =
    select
        [ on "change" (Json.map SetLanguage Util.targetValueIntParse) ]
        (Languages.map
            (\index name ->
                option
                    [ value (String.fromInt index)
                    , selected (model.languageIndex == index)
                    ]
                    [ text name ]
            )
        )


languageArea model =
    case Languages.code model.name model.languageIndex model.alignment of
        Err theErr ->
            [ div [] [ text theErr ] ]

        Ok ( theBytes, theCode ) ->
            [ h3 [] [ text "Hex bytes" ]
            , pre [] [ text (theBytes |> Bytes.spaceSeparatedHexDump) ]
            , h3 [] [ text "Source code" ]
            , languageDropdown model
            , pre [] [ text theCode ]
            ]


view : Model -> Html.Html Msg
view model =
    div []
        (List.append
            [ h3 [] [ text "Your signature" ]
            , input
                [ value model.name
                , size 50
                , onInput SetName
                ]
                []
            , alignmentDropdown model
            ]
            (languageArea model)
        )


update msg model =
    ( case msg of
        Initialize t ->
            { model | name = initialNameWithTime t }

        SetName name ->
            { model | name = name }

        SetLanguage languageIndex ->
            { model | languageIndex = languageIndex }

        SetAlignment alignment ->
            { model | alignment = alignment }
    , Cmd.none
    )


subscriptions number =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
