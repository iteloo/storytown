module TransView exposing (view)

import Message exposing (..)
import MyCss exposing (CssClass(..))
import Trans exposing (..)
import Parser exposing (..)
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))
import Bootstrap.Button as Button
import Html.CssHelpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace MyCss.storytown


view : Collapsable String Word -> Html StoryMsg
view collapsable =
    div [ class [ Table ] ]
        [ collapsableView (initViewCollapsable collapsable)
        ]


initViewCollapsable :
    Collapsable String Word
    -> Collapsable ( String, Maybe (CursorZipper String Word) ) Word
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


collapsableView :
    Collapsable ( String, Maybe (CursorZipper String Word) ) Word
    -> Html StoryMsg
collapsableView collapsable =
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


genericBlockView :
    ( String, Maybe (CursorZipper String Word) )
    -> List (Html StoryMsg)
    -> Html StoryMsg
genericBlockView ( tr, z ) childViews =
    div [ class [ Cell ] ]
        [ div [ class [ Row ] ]
            [ div [] childViews ]
        , div [ class [ SidePadding ] ]
            [ div
                [ class <|
                    addHasExpand z <|
                        addMin z [ Hoverarea ]
                ]
              <|
                addCollapse z <|
                    addExpand z <|
                        [ div [ class [ Padding ] ]
                            [ div [ class [ Trans ] ]
                                [ text tr ]
                            ]
                        ]
            ]
        ]


wordView w =
    span [ class [ Cell, Orig ] ] [ text (w ++ " ") ]



--  let
--     txt =
--         a
--             (if
--                 isPaused playbackState
--                     || isPlaying playbackState
--              then
--                 [ onClick (TextClicked index) ]
--              else
--                 []
--             )
--             [ textarea
--                 [ placeholder "Write something..."
--                 , onInput (ItemSourceChange index)
--                 ]
--                 [ text item.text ]
--             ]
--   in
--     if
--         currentItemState playbackState
--             |> Maybe.map ((==) index << .itemId)
--             |> Maybe.withDefault False
--     then
--         mark []
--             [ txt ]
--     else
--         txt


addMin z =
    case z of
        Nothing ->
            (::) Min

        Just _ ->
            identity


addHasExpand z =
    case z |> Maybe.andThen Trans.expand of
        Nothing ->
            identity

        Just _ ->
            (::) HasExpand


addExpand z =
    case z |> Maybe.andThen Trans.expand of
        Nothing ->
            identity

        Just z ->
            (::)
                (div
                    [ class [ Expand ]
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
                    [ class [ Collapse ]
                    , onClick <|
                        CollapsableChange <|
                            underlyingCollapsable z
                    ]
                    [ text "^" ]
                ]
