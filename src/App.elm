module App exposing (..)

import Browser
import Html exposing (div, textarea, text, h1, h2, h3)
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
        , div [] <| parseDatShiz model
        ]


parseDatShiz model =
    let
        result =
            run parseMarkDowns model
    in
        case result of
            Err e ->
                [ text <| model ]

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
                                text element.text
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
            |. keyword "***"
        , succeed H2
            |. keyword "**"
        , succeed H1
            |. symbol "*"
        ]


chompLine =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> c /= '\n')


parseSingleEl =
    succeed Element
        |= selector
        |. symbol " "
        |= chompLine


elListHelp : List Element -> Parser (Step (List Element) (List Element))
elListHelp els =
    oneOf
        [ succeed (\el -> Loop (el :: els))
            |= parseSingleEl
            |. symbol "\n"
        , succeed ()
            |> map (\_ -> Done (List.reverse els))
        ]


parseMarkDowns : Parser (List Element)
parseMarkDowns =
    loop [] elListHelp



-- succeed Elements
--     |. symbol "("
--     |. spaces
--     |= float
--     |. spaces
--     |. symbol ","
--     |. spaces
--     |= float
--     |. spaces
--     |. symbol ")"


update msg model =
    case msg of
        Update text ->
            text


main =
    Browser.sandbox { view = view, init = init, update = update }
