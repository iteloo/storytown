module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (html, body)
import Css.Namespace exposing (namespace)


storytown : String
storytown =
    "storytown"


type CssClass
    = Footer
    | Flex
    | Table
    | Row
    | Cell
    | Orig
    | FakeTable
    | FakeRow
    | FakeCell
    | MeasurementDiv
    | SentenceMeasurementDiv
    | MeasurementSpan
    | SidePadding
    | Padding
    | Trans
    | Hoverarea
    | Hover
    | Min
    | Expand
    | Collapse
    | ColouredSpace
    | Marked


css : Stylesheet
css =
    (stylesheet << namespace storytown) <|
        let
            origFontSize =
                pt 24

            transFontSize =
                pt 10

            green =
                rgba 0 127 0

            blueGreen =
                rgba 0 127 127

            orange =
                rgba 255 165 0

            expandCollapse =
                [ position absolute
                , backgroundColor (blueGreen 0.05)
                , height expandCollapseHeight
                , fontSize (pt 8)
                , lineHeight (num 1)
                , hover
                    [ backgroundColor (green 0.2) ]
                , width (pct 100)
                ]

            footerHeight =
                auto

            expandCollapseHeight =
                pt 9
        in
            [ html
                [ position relative
                , minHeight (pct 100)
                ]
            , body
                [ marginBottom footerHeight
                ]
            , class Footer
                [ position absolute
                , bottom zero
                , width (pct 100)
                , height footerHeight
                , backgroundColor (hex "#f5f5f5")
                ]
            , class Flex
                [ property "display" "flex"
                ]
            , class Table
                [ display table
                , borderCollapse collapse
                ]
            , class Row
                [ display tableRow ]
            , class Cell
                [ display tableCell
                , textAlign center
                ]
            , class FakeTable
                [ whiteSpace noWrap ]
            , class FakeRow
                []
            , class FakeCell
                [ display inlineBlock
                , verticalAlign top
                ]
            , class Orig
                [ fontSize origFontSize
                , property "white-space" "pre"
                , textAlign left
                ]
            , class MeasurementDiv
                [ property "visibility" "hidden"
                , fontSize origFontSize
                , textAlign left
                ]
            , class SentenceMeasurementDiv
                [ property "visibility" "hidden"
                , fontSize transFontSize
                , whiteSpace noWrap
                , textAlign left
                ]
            , class MeasurementSpan
                [ whiteSpace noWrap ]
            , class SidePadding
                [ paddingLeft (pt 2)
                , paddingRight (pt 2)
                , textAlign center
                ]
            , class Padding
                [ borderTop3 (px 1.3) solid (green 0.4)
                , paddingBottom (pt 1.2)
                , paddingTop (pt 1.2)
                ]
            , class Trans
                [ color (green 0.7)
                , fontSize transFontSize
                , empty
                    [ before
                        [ property "content" "\"\\200b\"" ]
                    ]
                ]
            , class Hoverarea <|
                let
                    displayNone =
                        [ display none ]

                    displayBlock =
                        [ display block ]
                in
                    [ boxSizing borderBox
                    , position relative
                      -- , maxHeight zero
                    , withClass Min
                        [ borderTop zero
                        , maxHeight zero
                        , overflow hidden
                        ]
                    , withClass Hover
                        [ children
                            [ class Padding
                                [ backgroundColor (green 0.1) ]
                            , class Expand displayBlock
                            , class Collapse displayBlock
                            ]
                        ]
                    , children
                        [ class Expand displayNone
                        , class Collapse displayNone
                        ]
                    ]
            , class Expand <|
                List.concat
                    [ [ transform (translateY (pt 0 |-| expandCollapseHeight)) ]
                    , expandCollapse
                    ]
            , class Collapse expandCollapse
            , class ColouredSpace
                [ backgroundColor (orange 0.2)
                ]
            , class Marked
                [ backgroundColor (orange 0.2) ]
            ]
