module TransView exposing (view)

import Message exposing (..)
import MyCss exposing (CssClass(..))
import Translation.Base exposing (..)
import Translation.Cursor exposing (..)
import Translation.Block exposing (..)
import Translation.Layout exposing (..)
import Translation.Path exposing (..)
import Helper
import Helper.State2
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import Dict
import Css
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


{ id, class, classList } =
    Html.CssHelpers.withNamespace MyCss.storytown


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


view : ParagraphLayout -> Html StoryMsg
view layout =
    case layout of
        Raw ( raw, ellipses ) ->
            div [] <|
                List.concat
                    [ Either.fromEither
                        (List.singleton << wordsMeasureDiv)
                        (always [])
                        raw
                    , Either.fromEither
                        paragraphMeasureDivs
                        paragraphMeasureDivs
                        raw
                    , case ellipses of
                        Nothing ->
                            -- [tmp] hard-coded
                            [ measureDiv EllipsesMeasure [ "..." ] ]

                        Just _ ->
                            []
                    ]

        Formatted formatted ->
            { formatted
                | paragraph =
                    Tuple.mapFirst
                        (Dict.map
                            (\_ i ->
                                { i | collapsable = registerCursors i.collapsable }
                            )
                        )
                        formatted.paragraph
            }
                |> splitParagraph

        LayoutError e ->
            -- [todo] handle more gracefully
            Debug.crash ("layout error: " ++ toString e)


paragraphMeasureDivs :
    Paragraph (Either (List String) (List (Measured String))) b
    -> List (Html StoryMsg)
paragraphMeasureDivs =
    List.concatMap
        (\( idx, sen ) ->
            nodes <|
                pathedMap (measureDiv << TransMeasure << ((,) idx)) <|
                    mapCollapsable
                        (Either.fromEither identity (List.map .content))
                        identity
                        sen.collapsable
        )
        << Dict.toList


wordsMeasureDiv : Paragraph a Word -> Html msg
wordsMeasureDiv =
    let
        nontermBlock _ =
            List.concat
                << AtLeastOneOf.toList
                << AtLeastOneOf.map identity List.singleton
    in
        measureDiv WordsMeasure
            << List.concatMap
                (foldr
                    List.singleton
                    identity
                    identity
                    nontermBlock
                    (\_ ->
                        List.concat
                            << Nonempty.toList
                            << Nonempty.map List.singleton
                    )
                    nontermBlock
                    << .collapsable
                )
            << Dict.values


measureDiv : Measure -> List String -> Html msg
measureDiv m =
    div
        [ Html.Attributes.id (toDivId m)
        , class
            [ case m of
                TransMeasure _ ->
                    SentenceMeasurementDiv

                WordsMeasure ->
                    MeasurementDiv

                EllipsesMeasure ->
                    SentenceMeasurementDiv
            ]
        ]
        << List.map (span [] << List.singleton << text)


splitParagraph :
    { paragraph : ( Paragraph ( List (Measured String), Maybe (CursorZipper (List (Measured String)) (Measured Word)) ) (Measured Word), Measured String )
    , hover : Maybe FullPath
    }
    -> Html StoryMsg
splitParagraph { paragraph, hover } =
    let
        ( para, ellipses ) =
            paragraph
    in
        div [ class [ FakeTable ] ]
            << List.map
                (div [ class [ FakeRow ] ]
                    << Nonempty.toList
                    << Nonempty.map .content
                )
            << Helper.truncateListAfter .isEnd
            << List.concat
            << Dict.values
            << Dict.map
                (\idx ->
                    Nonempty.toList
                        << splitCollapsable ellipses
                            idx
                            (hover
                                |> Maybe.andThen
                                    (\( idxpath, path ) ->
                                        if idxpath == idx then
                                            Just path
                                        else
                                            Nothing
                                    )
                            )
                        << .collapsable
                )
        <|
            para


splitCollapsable :
    Measured String
    -> Int
    -> Maybe Path
    -> Collapsable ( List (Measured String), Maybe (CursorZipper (List (Measured String)) (Measured Word)) ) (Measured Word)
    -> Nonempty (Measured (Html StoryMsg))
splitCollapsable ellipses idx hover =
    let
        wordView : Measured Word -> Nonempty (Measured (Html msg))
        wordView w =
            Nonempty.fromElement <|
                { w
                    | content =
                        span
                            [ class [ FakeCell, Orig ]
                            , styles [ Css.width (Css.px w.width) ]
                            ]
                            [ text w.content ]
                }

        splitTrans :
            ( Path, ( List (Measured String), Maybe (CursorZipper (List (Measured String)) (Measured Word)) ) )
            -> Nonempty (Nonempty (Measured (Html StoryMsg)))
            -> Nonempty (Measured (Html StoryMsg))
        splitTrans ( path, ( trs, z ) ) =
            Nonempty.indexedMap
                (\i ->
                    let
                        mon :
                            Nonempty (Measured (Html StoryMsg))
                            -> List (Measured String)
                            -> ( Measured (Html StoryMsg), List (Measured String) )
                        mon cs rem =
                            let
                                cwdt =
                                    (-) (cs |> Nonempty.toList |> List.map .width |> List.sum)
                                        ((if i == 0 then
                                            1
                                          else
                                            2
                                         )
                                            * ellipses.width
                                        )

                                prependEllipses trs =
                                    if i == 0 || List.isEmpty trs then
                                        trs
                                    else
                                        ellipses :: trs

                                go :
                                    List (Measured String)
                                    -> Float
                                    -> List (Measured String)
                                    -> ( List (Measured String), List (Measured String) )
                                go revtrs w rem =
                                    case rem of
                                        [] ->
                                            ( prependEllipses (List.reverse revtrs), rem )

                                        tr :: rem_ ->
                                            if tr.width + w < cwdt then
                                                go (tr :: revtrs) (tr.width + w) rem_
                                            else
                                                ( prependEllipses (List.reverse (ellipses :: revtrs)), rem )
                            in
                                go [] 0 rem
                                    |> Tuple.mapFirst
                                        (\trs -> mkBlock path ( trs, z ) cs)
                    in
                        mon
                )
                >> traverseNonempty
                >> Helper.State2.runState trs
                >> Tuple.first

        mkBlock :
            Path
            -> ( List (Measured String), Maybe (CursorZipper (List (Measured String)) (Measured Word)) )
            -> Nonempty (Measured (Html StoryMsg))
            -> Measured (Html StoryMsg)
        mkBlock path trz mbs =
            let
                width =
                    List.sum (List.map .width <| Nonempty.toList mbs)
            in
                { content =
                    genericBlockView
                        idx
                        path
                        (hover
                            |> Maybe.map ((==) path)
                            |> Maybe.withDefault False
                        )
                        -- [tmp] doesn't split trans
                        (Tuple.mapFirst
                            (List.foldr (++) "" << List.map .content)
                            trz
                        )
                        width
                        (List.map .content <| Nonempty.toList <| mbs)
                , width = width
                , isEnd = .isEnd (Helper.nonemptyLast mbs)
                }

        expandedBlock :
            ( Path, ( List (Measured String), Maybe (CursorZipper (List (Measured String)) (Measured Word)) ) )
            -> AtLeastOneOf (Nonempty (Measured (Html StoryMsg))) (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        expandedBlock trzs =
            splitTrans trzs
                << Helper.truncateAfter .isEnd
                << Nonempty.concat
                << AtLeastOneOf.toNonempty
                << AtLeastOneOf.map identity wordView

        terminalBlock :
            ( Path, ( List (Measured String), Maybe (CursorZipper (List (Measured String)) (Measured Word)) ) )
            -> Nonempty (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        terminalBlock trzs =
            splitTrans trzs
                << Helper.truncateAfter .isEnd
                << Nonempty.concatMap wordView

        -- [note] identical to expandedBlock right now
        collapsedBlock :
            ( Path, ( List (Measured String), Maybe (CursorZipper (List (Measured String)) (Measured Word)) ) )
            -> AtLeastOneOf (Nonempty (Measured (Html StoryMsg))) (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        collapsedBlock trzs =
            splitTrans trzs
                << Helper.truncateAfter .isEnd
                << Nonempty.concat
                << AtLeastOneOf.toNonempty
                << AtLeastOneOf.map identity wordView
    in
        foldr
            wordView
            identity
            identity
            expandedBlock
            terminalBlock
            collapsedBlock
            << pathedMap (,)


{-| [todo] move this into the main view code to avoid using Maybe
-}
registerCursors :
    Collapsable a b
    -> Collapsable ( a, Maybe (CursorZipper a b) ) b
registerCursors =
    let
        register :
            CursorZipper ( a, Maybe (CursorZipper a b) ) b
            -> CursorZipper ( a, Maybe (CursorZipper a b) ) b
        register z =
            updateNodeCursorZipper
                (Tuple.mapSecond
                    (always (Just (mapCursorZipper Tuple.first identity z)))
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
            >> Either.fromEither LoneLeaf Block


genericBlockView :
    Int
    -> Path
    -> Bool
    -> ( String, Maybe (CursorZipper (List (Measured String)) (Measured Word)) )
    -> Float
    -> List (Html StoryMsg)
    -> Html StoryMsg
genericBlockView idx path isHover ( tr, z ) width childViews =
    div
        [ class [ FakeCell ]
        , styles [ Css.width (Css.px width) ]
        ]
        [ div [ class [ FakeRow ] ] <|
            List.concat
                [ [ div [] childViews ]
                , case z of
                    Nothing ->
                        []

                    Just _ ->
                        [ div [ class [ SidePadding ] ]
                            [ div
                                (List.concat
                                    [ [ class <|
                                            addMin z <|
                                                List.concat
                                                    [ [ Hoverarea ]
                                                    , if isHover then
                                                        [ Hover ]
                                                      else
                                                        []
                                                    ]
                                      , onMouseEnter (MouseEnter ( idx, path ))
                                      ]
                                    , if isHover then
                                        [ onMouseLeave MouseLeave ]
                                      else
                                        []
                                    ]
                                )
                              <|
                                addCollapse idx z <|
                                    addExpand idx z <|
                                        [ div [ class [ Padding ] ]
                                            [ div [ class [ Trans ] ]
                                                [ text tr ]
                                            ]
                                        ]
                            ]
                        ]
                ]
        ]



-- [todo] incorporate this function
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


addMin : Maybe a -> List CssClass -> List CssClass
addMin z =
    case z of
        Nothing ->
            (::) Min

        Just _ ->
            identity


addExpand :
    Int
    -> Maybe (CursorZipper (List (Measured String)) (Measured Word))
    -> List (Html StoryMsg)
    -> List (Html StoryMsg)
addExpand idx z =
    case z |> Maybe.andThen expand of
        Nothing ->
            identity

        Just z ->
            (::)
                (div
                    [ class [ Expand ]
                    , onClick <|
                        CollapsableChange idx <|
                            Translation.Cursor.underlyingCollapsable z
                    ]
                    [ text "v" ]
                )


addCollapse :
    Int
    -> Maybe (CursorZipper (List (Measured String)) (Measured Word))
    -> List (Html StoryMsg)
    -> List (Html StoryMsg)
addCollapse idx z =
    case z |> Maybe.andThen collapse of
        Nothing ->
            identity

        Just z ->
            flip (++)
                [ div
                    [ class [ Collapse ]
                    , onClick <|
                        CollapsableChange idx <|
                            Translation.Cursor.underlyingCollapsable z
                    ]
                    [ text "^" ]
                ]
