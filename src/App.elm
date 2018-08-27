module App exposing (..)

import Browser
import Html exposing (div, textarea, text, h1, h2, h3, div)
import Html.Events exposing (..)
import Html.Attributes exposing (value, style)
import Parser exposing (..)


init : String
init =
    ""


view model =
    div []
        [ textarea
            [ value model
            , onInput Update
            , style "width" "100%"
            , style "height" "50vh"
            , style "position" "relative"
            , style "box-shadow" "0 0 5px black inset"
            , style "padding" "10px"
            , style "box-sizing" "border-box"
            , style "outline" "0"
            ]
            []
        , div [] <| markdownToHtml model
        ]


deadEndToString : Problem -> String
deadEndToString problem =
    case problem of
        ExpectingInt ->
            "ExpectingInt"

        ExpectingHex ->
            "ExpectingHex"

        ExpectingOctal ->
            "ExpectingOctal"

        ExpectingBinary ->
            "ExpectingBinary"

        ExpectingFloat ->
            "ExpectingFloat"

        ExpectingNumber ->
            "ExpectingNumber"

        ExpectingVariable ->
            "ExpectingVariable"

        ExpectingSymbol str ->
            "ExpectingSymbol " ++ str

        ExpectingKeyword str ->
            "ExpectingKeyword " ++ str

        ExpectingEnd ->
            "ExpectingEnd"

        UnexpectedChar ->
            "UnexpectedChar"

        Problem str ->
            "Problem " ++ str

        BadRepeat ->
            "BadRepeat"

        Expecting str ->
            "Expecting " ++ str


deadEndConversion : DeadEnd -> Html.Html msg
deadEndConversion deadEnd =
    div [] [ text <| deadEndToString deadEnd.problem ]


markdownToHtml model =
    let
        result =
            run parseMarkDowns model
    in
        case result of
            Err e ->
                List.map deadEndConversion e

            Ok elements ->
                List.map
                    (\element ->
                        case element.elType of
                            H1 ->
                                h1 [] [ text element.text ]

                            H2 ->
                                h2 [] [ text element.text ]

                            H3 ->
                                h3 [] [ text element.text ]

                            Text ->
                                div [] [ text element.text ]
                    )
                    elements


type Msg
    = Update String


type El
    = H1
    | H2
    | H3
    | Text


type alias Element =
    { elType : El
    , text : String
    }


selector =
    oneOf
        [ succeed H3
            |. keyword "###"
        , succeed H2
            |. keyword "##"
        , succeed H1
            |. symbol "#"
        ]


chompLine =
    getChompedString <|
        succeed ()
            |. chompUntilEndOr "\n"


parseSingleEl =
    succeed Element
        |= selector
        |. symbol " "
        |= chompLine


parseText =
    succeed Element
        |= succeed Text
        |= chompLine


elListHelp : List Element -> Parser (Step (List Element) (List Element))
elListHelp els =
    let
        doneWhen =
            map (\_ -> Done (List.reverse els))

        onNext =
            (\el -> Loop (el :: els))
    in
        oneOf
            [ doneWhen end
            , succeed (\_ -> Loop els)
                |= token "\n"
            , succeed onNext
                |= parseSingleEl
            , succeed onNext
                |= parseText
            ]


parseMarkDowns : Parser (List Element)
parseMarkDowns =
    loop [] elListHelp


update msg model =
    case msg of
        Update text ->
            text


main =
    Browser.sandbox { view = view, init = init, update = update }
