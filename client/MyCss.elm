module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (html, body)
import Css.Namespace exposing (namespace)


storytown =
    "storytown"


type CssClass
    = Footer
    | Table
    | Row
    | Cell
    | Orig
    | SidePadding
    | Padding
    | Trans
    | Hoverarea
    | Min
    | HasExpand
    | Expand
    | Collapse


css =
    (stylesheet << namespace storytown) <|
        let
            expandCollapse =
                let
                    onHover =
                        [ backgroundColor (rgba 0 127 127 0.2) ]
                in
                    [ backgroundColor (rgba 0 127 127 0.05)
                    , height expandCollapseHeight
                    , fontSize (pt 8)
                    , lineHeight (num 1)
                    , hover onHover
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
            , class Table
                [ display table
                , borderCollapse collapse
                ]
            , class Row
                [ display tableRow ]
            , class Cell
                [ display tableCell
                , textAlign center
                , withClass Orig
                    [ fontSize (pt 36)
                    , property "white-space" "pre"
                    ]
                ]
            , class SidePadding
                [ paddingLeft (pt 2)
                , paddingRight (pt 2)
                , textAlign center
                ]
            , class Padding
                [ borderTop3 (px 1.3) solid (rgba 0 127 0 0.4)
                , paddingBottom (pt 1.2)
                , paddingTop (pt 1.2)
                ]
            , class Trans
                [ color (rgba 0 127 0 0.7) ]
            , class Hoverarea <|
                let
                    displayNone =
                        [ display none ]

                    displayBlock =
                        [ display block ]
                in
                    [ boxSizing borderBox
                      -- , maxHeight zero
                    , withClass Min
                        [ borderTop zero
                        , maxHeight zero
                        , overflow hidden
                        ]
                    , withClass HasExpand
                        [ hover
                            [ transform
                                (translateY (pt 0 |-| expandCollapseHeight))
                            ]
                        ]
                    , hover
                        [ children
                            [ class Padding
                                [ backgroundColor (rgba 0 127 0 0.1) ]
                            , class Expand displayBlock
                            , class Collapse displayBlock
                            ]
                        ]
                    , children
                        [ class Expand displayNone
                        , class Collapse displayNone
                        ]
                    ]
            , class Expand expandCollapse
            , class Collapse expandCollapse
            ]
