module TransView exposing (..)

import Message exposing (..)
import Trans exposing (..)
import Parser exposing (..)
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))
import Bootstrap.Button as Button


initViewCollapsable : Collapsable String Word -> Collapsable ( String, Maybe (CursorZipper String Word) ) Word
initViewCollapsable =
    let
        register :
            CursorZipper ( a, Maybe (CursorZipper a b) ) b
            -> CursorZipper ( a, Maybe (CursorZipper a b) ) b
        register z =
            updateNodeCursorZipper
                (\( a, _ ) ->
                    ( a, Just (mapCursorZipper (\( a, _ ) -> a) identity z) )
                )
                z
    in
        initCursorZipper
            >> Either.mapRight
                (mapCursorZipper (\str -> ( str, Nothing )) identity
                    >> register
                    >> untilNothing
                        (rightBottom >> Maybe.map register)
                    >> untilNothing
                        (leftBottom >> Maybe.map register)
                    >> toBlockZipper
                    >> top
                    >> (\(BlockZipper block ctx) -> block)
                )
            >> Either.fromEither LoneWord Block


view : Collapsable String Word -> Html StoryEditMsg
view =
    initViewCollapsable >> collapsableView >> (\v -> div [ class "table" ] [ v ])


collapsableView :
    Collapsable ( String, Maybe (CursorZipper String Word) ) Word
    -> Html StoryEditMsg
collapsableView collapsable =
    let
        addMin z =
            case z of
                Nothing ->
                    (++) "min "

                Just _ ->
                    identity

        addHasExpand z =
            case z |> Maybe.andThen Trans.expand of
                Nothing ->
                    identity

                Just _ ->
                    (++) "hasexpand "

        addExpand z =
            case z |> Maybe.andThen Trans.expand of
                Nothing ->
                    identity

                Just z ->
                    (::)
                        (div
                            [ class "expand"
                            , onClick <|
                                CollapsableChange <|
                                    underlyingCollapsable z
                            ]
                            [ text "v" ]
                        )

        addCollapse z =
            case z |> Maybe.andThen Trans.collapse of
                Nothing ->
                    identity

                Just z ->
                    flip (++)
                        [ div
                            [ class "collapse"
                            , onClick <|
                                CollapsableChange <|
                                    underlyingCollapsable z
                            ]
                            [ text "^" ]
                        ]

        genericBlockView :
            ( String, Maybe (CursorZipper String Word) )
            -> List (Html StoryEditMsg)
            -> Html StoryEditMsg
        genericBlockView ( tr, z ) childViews =
            div [ class "cell" ]
                [ div [ class "row" ]
                    [ div [] childViews ]
                , div [ class "side-padding" ]
                    [ div
                        [ class <|
                            addHasExpand z <|
                                addMin z "hoverarea"
                        ]
                      <|
                        addCollapse z <|
                            addExpand z <|
                                [ div [ class "padding" ]
                                    [ div [ class "trans" ]
                                        [ text tr ]
                                    ]
                                ]
                    ]
                ]

        wordView w =
            span [ class "cell orig" ] [ text w ]
    in
        case collapsable of
            LoneWord w ->
                wordView w

            Block block ->
                let
                    blockView block =
                        case block of
                            ExpandedBlock trz bs ->
                                genericBlockView trz <|
                                    List.map collapsableView <|
                                        AtLeastOneOf.toList <|
                                            AtLeastOneOf.map
                                                Block
                                                LoneWord
                                                bs

                            CursorBlock cblock ->
                                let
                                    cursorBlockView cblock =
                                        case cblock of
                                            TerminalBlock trz ws ->
                                                genericBlockView trz <|
                                                    List.map wordView <|
                                                        Nonempty.toList ws

                                            CollapsedBlock trz bs ->
                                                genericBlockView trz <|
                                                    AtLeastOneOf.toList <|
                                                        AtLeastOneOf.map
                                                            cursorBlockView
                                                            wordView
                                                            bs
                                in
                                    cursorBlockView cblock
                in
                    blockView block



-- TEST


testTrans =
    TranslatedBlock
        (Nonempty
            (TranslatedBlock
                (Nonempty (L2Word "what!") [])
                "什麼！"
            )
            [ (TranslatedBlock
                (Nonempty
                    (L2Word "he")
                    [ TranslatedBlock
                        (Nonempty (L2Word "is") [ L2Word "alive" ])
                        "活著"
                    ]
                )
                "他活著"
              )
            ]
        )
        "什麼！他還活著！？"


test =
    testTrans |> fullyCollapsed
