module TransView exposing (view)

import Model exposing (..)
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
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


{ id, class, classList } =
    Html.CssHelpers.withNamespace MyCss.storytown


view : Int -> CollapsableLayout -> Html StoryMsg
view idx colLayout =
    case colLayout of
        Raw rawCol ->
            measureDiv idx rawCol

        Formatted col ->
            splitCollapsable <|
                registerCursors col

        LayoutError e ->
            -- [todo] handle more gracefully
            Debug.crash ("layout error: " ++ toString e)


measureDiv : Int -> Collapsable a Word -> Html msg
measureDiv idx =
    let
        singleWordSpan w =
            [ span [] [ text w ] ]

        nontermBlock =
            (\_ ->
                List.concat
                    << AtLeastOneOf.toList
                    << AtLeastOneOf.map identity singleWordSpan
            )
    in
        div
            [ Html.Attributes.id ("measureDiv" ++ toString idx)
            , class [ MeasurementDiv ]
            ]
            << foldr
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


splitCollapsable :
    Collapsable ( String, Maybe (CursorZipper String (Measured Word)) ) (Measured Word)
    -> Html StoryMsg
splitCollapsable =
    let
        expandedBlock :
            ( String, Maybe (CursorZipper String (Measured Word)) )
            -> AtLeastOneOf (Nonempty (Measured (Html StoryMsg))) (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        expandedBlock trz =
            Nonempty.map
                (\hwis ->
                    { content =
                        genericBlockView trz
                            (List.map .content <| Nonempty.toList hwis)
                    , width =
                        List.sum
                            (List.map .width <| Nonempty.toList hwis)
                    , isEnd = .isEnd (Helper.nonemptyLast hwis)
                    }
                )
                << Helper.truncateAfter .isEnd
                << Nonempty.concat
                << AtLeastOneOf.toNonempty
                << AtLeastOneOf.map identity
                    (\w ->
                        Nonempty.fromElement
                            { w | content = wordView w.content }
                    )

        terminalBlock :
            ( String, Maybe (CursorZipper String (Measured Word)) )
            -> Nonempty (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        terminalBlock trz =
            Nonempty.map
                (\twis ->
                    { content =
                        genericBlockView trz
                            (List.map (wordView << .content) <|
                                Nonempty.toList twis
                            )
                    , width =
                        List.sum
                            (List.map .width <| Nonempty.toList twis)
                    , isEnd = .isEnd (Helper.nonemptyLast twis)
                    }
                )
                << Helper.truncateAfter .isEnd

        -- [note] identical to expandedBlock right now
        collapsedBlock :
            ( String, Maybe (CursorZipper String (Measured Word)) )
            -> AtLeastOneOf (Nonempty (Measured (Html StoryMsg))) (Measured Word)
            -> Nonempty (Measured (Html StoryMsg))
        collapsedBlock trz =
            Nonempty.map
                (\hwis ->
                    { content =
                        genericBlockView trz
                            (List.map .content <| Nonempty.toList hwis)
                    , width =
                        List.sum
                            (List.map .width <| Nonempty.toList hwis)
                    , isEnd = .isEnd (Helper.nonemptyLast hwis)
                    }
                )
                << Helper.truncateAfter .isEnd
                << Nonempty.concat
                << AtLeastOneOf.toNonempty
                << AtLeastOneOf.map identity
                    (\w ->
                        Nonempty.fromElement
                            { w | content = wordView w.content }
                    )
    in
        div [ class [ Table ] ]
            << List.map (div [ class [ Row ] ] << List.singleton)
            << foldr
                (List.singleton << wordView << .content)
                (Nonempty.toList << Nonempty.map .content)
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
            >> Either.fromEither LoneLeaf Block


genericBlockView :
    ( String, Maybe (CursorZipper String (Measured Word)) )
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


wordView : Word -> Html msg
wordView w =
    span [ class [ Cell, Orig ] ] [ text w ]



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
    Maybe (CursorZipper String (Measured Word))
    -> List (Html StoryMsg)
    -> List (Html StoryMsg)
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


addCollapse :
    Maybe (CursorZipper String (Measured Word))
    -> List (Html StoryMsg)
    -> List (Html StoryMsg)
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
