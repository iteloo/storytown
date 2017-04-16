module TransView exposing (view)

import Message exposing (..)
import MyCss exposing (CssClass(..))
import Translation.Base exposing (..)
import Translation.Cursor exposing (..)
import Translation.Block exposing (..)
import Translation.Layout exposing (..)
import Helper
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


view : Int -> ParagraphLayout -> Html StoryMsg
view idx layout =
    case layout of
        Raw raw ->
            measureDiv idx raw

        Formatted para ->
            para
                |> Dict.map
                    (\_ i ->
                        { i | collapsable = registerCursors i.collapsable }
                    )
                |> splitParagraph

        LayoutError e ->
            -- [todo] handle more gracefully
            Debug.crash ("layout error: " ++ toString e)


measureDiv : Int -> Paragraph a Word -> Html msg
measureDiv _ =
    -- [note] unused first arg
    let
        singleWordSpan w =
            [ span [] [ text w ] ]

        nontermBlock _ =
            List.concat
                << AtLeastOneOf.toList
                << AtLeastOneOf.map identity singleWordSpan
    in
        div
            [ Html.Attributes.id "measureDiv0"
            , class [ MeasurementDiv ]
            ]
            << List.concatMap
                (foldr
                    singleWordSpan
                    identity
                    identity
                    nontermBlock
                    (\_ ->
                        List.concat
                            << Nonempty.toList
                            << Nonempty.map singleWordSpan
                    )
                    nontermBlock
                    << .collapsable
                )
            << Dict.values


splitParagraph :
    Paragraph ( String, Maybe (CursorZipper String (Measured Word)) ) (Measured Word)
    -> Html StoryMsg
splitParagraph =
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
                    << splitCollapsable idx
                    << .collapsable
            )


splitCollapsable :
    Int
    -> Collapsable ( String, Maybe (CursorZipper String (Measured Word)) ) (Measured Word)
    -> Nonempty (Measured (Html StoryMsg))
splitCollapsable idx =
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

        mkBlock trz mbs =
            let
                width =
                    List.sum (List.map .width <| Nonempty.toList mbs)
            in
                { content =
                    genericBlockView idx
                        trz
                        width
                        (List.map .content <| Nonempty.toList <| mbs)
                , width = width
                , isEnd = .isEnd (Helper.nonemptyLast mbs)
                }

        expandedBlock :
            ( String, Maybe (CursorZipper String (Measured Word)) )
            -> AtLeastOneOf (Nonempty (Measured (Html StoryMsg))) (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        expandedBlock trz =
            Nonempty.map (mkBlock trz)
                << Helper.truncateAfter .isEnd
                << Nonempty.concat
                << AtLeastOneOf.toNonempty
                << AtLeastOneOf.map identity wordView

        terminalBlock :
            ( String, Maybe (CursorZipper String (Measured Word)) )
            -> Nonempty (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        terminalBlock trz =
            Nonempty.map (mkBlock trz)
                << Helper.truncateAfter .isEnd
                << Nonempty.concatMap wordView

        -- [note] identical to expandedBlock right now
        collapsedBlock :
            ( String, Maybe (CursorZipper String (Measured Word)) )
            -> AtLeastOneOf (Nonempty (Measured (Html StoryMsg))) (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        collapsedBlock trz =
            Nonempty.map (mkBlock trz)
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
    -> ( String, Maybe (CursorZipper String (Measured Word)) )
    -> Float
    -> List (Html StoryMsg)
    -> Html StoryMsg
genericBlockView idx ( tr, z ) width childViews =
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
                                [ class <|
                                    addHasExpand z <|
                                        addMin z [ Hoverarea ]
                                ]
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


addHasExpand : Maybe (CursorZipper a b) -> List CssClass -> List CssClass
addHasExpand z =
    case z |> Maybe.andThen expand of
        Nothing ->
            identity

        Just _ ->
            (::) HasExpand


addExpand :
    Int
    -> Maybe (CursorZipper String (Measured Word))
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
                            underlyingCollapsable z
                    ]
                    [ text "v" ]
                )


addCollapse :
    Int
    -> Maybe (CursorZipper String (Measured Word))
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
                            underlyingCollapsable z
                    ]
                    [ text "^" ]
                ]
