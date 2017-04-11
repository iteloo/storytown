module Translation.Cursor exposing (..)

{-| This translation module doesn't remember the cursors for collapsed subtree,
and each collapsed subtree is fully collapsed.
-}

import Translation.Base exposing (..)
import Translation.Block exposing (..)
import Parser exposing (TranslatedBlock(..))
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Helper
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


underlyingCollapsable : CursorZipper a b -> Collapsable a b
underlyingCollapsable =
    (\(BlockZipper block _) -> Block block) << top << toBlockZipper


initCursorZipper : Collapsable a b -> Either b (CursorZipper a b)
initCursorZipper collapsable =
    case collapsable of
        LoneWord w ->
            Left w

        Block block ->
            block |> initTop >> bottom |> Right


expand : CursorZipper a b -> Maybe (CursorZipper a b)
expand (CursorZipper focus ctx) =
    case focus of
        TerminalBlock _ _ ->
            Nothing

        CollapsedBlock a bs ->
            Just <|
                bottom
                    (BlockZipper
                        (ExpandedBlock a
                            (AtLeastOneOf.map CursorBlock identity bs)
                        )
                        ctx
                    )


collapse : CursorZipper a b -> Maybe (CursorZipper a b)
collapse (CursorZipper focus ctx) =
    case ctx of
        Top ->
            Nothing

        Down tr before after parctx ->
            Just <|
                CursorZipper
                    (CollapsedBlock tr
                        (case
                            AtLeastOneOf.fromList
                                (List.map fullyCollapse before)
                         of
                            Right ws ->
                                AtLeastOneOf ws
                                    focus
                                    (List.map fullyCollapse after)

                            Left (AtLeastOneOf before_ b_ after_) ->
                                AtLeastOneOf before_
                                    b_
                                    (after_
                                        ++ [ Left focus ]
                                        ++ (List.map fullyCollapse after)
                                    )
                        )
                    )
                    parctx


{-| multiple meanings in Nothing
-}
leftBottom : CursorZipper a b -> Maybe (CursorZipper a b)
leftBottom =
    toBlockZipper >> upTilLeftExists >> Maybe.map bottom


rightBottom : CursorZipper a b -> Maybe (CursorZipper a b)
rightBottom =
    toBlockZipper >> upTilRightExists >> Maybe.map bottom


toBlockZipper (CursorZipper bottom ctx) =
    BlockZipper (CursorBlock bottom) ctx
