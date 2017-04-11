module TransView exposing (view)

import Message exposing (..)
import MyCss exposing (CssClass(..))
import Overflow
import Translation.Base exposing (..)
import Translation.Cursor exposing (..)
import Translation.Block exposing (..)
import Translation.Leaf exposing (..)
import Parser exposing (..)
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Helper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))
import Bootstrap.Button as Button
import Html.CssHelpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace MyCss.storytown


foldLeaves :
    List c
    -> (b -> c -> d)
    -> Collapsable a b
    -> Maybe (Collapsable a d)
foldLeaves cs f col =
    let
        go : List c -> Collapsable a b -> Maybe ( Collapsable a d, List c )
        go cs col =
            let
                goLeaf : List c -> b -> Maybe ( d, List c )
                goLeaf cs b =
                    case cs of
                        [] ->
                            Nothing

                        c :: rem ->
                            Just ( f b c, rem )
            in
                case col of
                    LoneWord b ->
                        goLeaf cs b |> Maybe.map (Helper.mapFst LoneWord)

                    Block block ->
                        let
                            goBlock :
                                List c
                                -> Block a b
                                -> Maybe ( Block a d, List c )
                            goBlock cs block =
                                case block of
                                    ExpandedBlock tr (AtLeastOneOf bs a abs) ->
                                        List.foldl
                                            (\b ->
                                                Maybe.andThen
                                                    (\( bs, rem ) ->
                                                        goLeaf rem b
                                                            |> Maybe.map
                                                                (Helper.mapFst ((++) bs << List.singleton))
                                                    )
                                            )
                                            (Just ( [], cs ))
                                            bs
                                            |> Maybe.andThen
                                                (\( bs1, rem1 ) ->
                                                    goBlock rem1 a
                                                        |> Maybe.andThen
                                                            (\( a1, rem2 ) ->
                                                                List.foldl
                                                                    (\ab ->
                                                                        Maybe.andThen
                                                                            (\( abs, rem ) ->
                                                                                go rem (Either.fromEither Block LoneWord ab)
                                                                                    |> Maybe.map
                                                                                        (Helper.mapFst ((++) abs << List.singleton))
                                                                            )
                                                                    )
                                                                    (Just ( [], rem2 ))
                                                                    abs
                                                                    |> Maybe.map
                                                                        (Helper.mapFst
                                                                            (ExpandedBlock tr
                                                                                << AtLeastOneOf bs1 a1
                                                                                << List.map fromCollapsable
                                                                            )
                                                                        )
                                                            )
                                                )

                                    CursorBlock block ->
                                        let
                                            goCursorBlock :
                                                List c
                                                -> CursorBlock a b
                                                -> Maybe ( CursorBlock a d, List c )
                                            goCursorBlock cs block =
                                                case block of
                                                    CollapsedBlock tr (AtLeastOneOf bs a abs) ->
                                                        List.foldl
                                                            (\b ->
                                                                Maybe.andThen
                                                                    (\( bs, rem ) ->
                                                                        goLeaf rem b
                                                                            |> Maybe.map
                                                                                (Helper.mapFst ((++) bs << List.singleton))
                                                                    )
                                                            )
                                                            (Just ( [], cs ))
                                                            bs
                                                            |> Maybe.andThen
                                                                (\( bs1, rem1 ) ->
                                                                    goCursorBlock rem1 a
                                                                        |> Maybe.andThen
                                                                            (\( a1, rem2 ) ->
                                                                                List.foldl
                                                                                    (\ab ->
                                                                                        Maybe.andThen
                                                                                            (\( abs, rem ) ->
                                                                                                ab
                                                                                                    |> Either.fromEither
                                                                                                        (goCursorBlock rem
                                                                                                            >> Maybe.map (Helper.mapFst Left)
                                                                                                        )
                                                                                                        (goLeaf rem
                                                                                                            >> Maybe.map
                                                                                                                (Helper.mapFst Right)
                                                                                                        )
                                                                                                    |> Maybe.map
                                                                                                        (Helper.mapFst ((++) abs << List.singleton))
                                                                                            )
                                                                                    )
                                                                                    (Just ( [], rem2 ))
                                                                                    abs
                                                                                    |> Maybe.map
                                                                                        (Helper.mapFst
                                                                                            (CollapsedBlock tr
                                                                                                << AtLeastOneOf bs1 a1
                                                                                            )
                                                                                        )
                                                                            )
                                                                )

                                                    TerminalBlock a (Nonempty b bs) ->
                                                        List.foldl
                                                            (\b ->
                                                                Maybe.andThen
                                                                    (\( bs, rem ) ->
                                                                        goLeaf rem b
                                                                            |> Maybe.map
                                                                                (Helper.mapFst
                                                                                    (Nonempty.append bs
                                                                                        << Nonempty.fromElement
                                                                                    )
                                                                                )
                                                                    )
                                                            )
                                                            (goLeaf cs b |> Maybe.map (Helper.mapFst Nonempty.fromElement))
                                                            bs
                                                            |> Maybe.map (Helper.mapFst (TerminalBlock a))
                                        in
                                            goCursorBlock cs block
                                                |> Maybe.map (Helper.mapFst CursorBlock)
                        in
                            goBlock cs block
                                |> Maybe.map (Helper.mapFst Block)
    in
        go cs col
            |> Maybe.andThen
                (\( col, rem ) ->
                    if List.isEmpty rem then
                        Just col
                    else
                        Nothing
                )



-- OLD


view : Collapsable String Word -> Html StoryMsg
view col =
    case foldLeaves [ 1, 2, 3, 4, 5, 6, 7 ] (,) col of
        Nothing ->
            text "error calculating widths"

        Just col ->
            view1 (mapCollapsable identity toString col)


view1 : Collapsable String Word -> Html StoryMsg
view1 collapsable =
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
                    >> Helper.untilNothing
                        (rightBottom >> Maybe.map register)
                    >> Helper.untilNothing
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
    case z |> Maybe.andThen expand of
        Nothing ->
            identity

        Just _ ->
            (::) HasExpand


addExpand z =
    case z |> Maybe.andThen expand of
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
    case z |> Maybe.andThen collapse of
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
