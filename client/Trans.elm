module Trans exposing (..)

{-| This translation module doesn't remember the cursors for collapsed subtree,
and each collapsed subtree is fully collapsed.
-}

import Parser exposing (TranslatedBlock(..))
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


-- TYPES


type alias Word =
    String


type Collapsable a b
    = LoneWord b
    | Block (Block a b)


mapCollapsable f g c =
    case c of
        LoneWord b ->
            LoneWord (g b)

        Block block ->
            Block (mapBlock f g block)


type Block a b
    = CursorBlock (CursorBlock a b)
    | ExpandedBlock a (AtLeastOneOf (Block a b) b)


mapBlock : (a -> c) -> (b -> d) -> Block a b -> Block c d
mapBlock f g block =
    case block of
        CursorBlock block ->
            CursorBlock (mapCursorBlock f g block)

        ExpandedBlock a bs ->
            ExpandedBlock (f a) (AtLeastOneOf.map (mapBlock f g) g bs)


getNodeBlock block =
    case block of
        ExpandedBlock a _ ->
            a

        CursorBlock cblock ->
            getNodeCursorBlock cblock


type CursorBlock a b
    = TerminalBlock a (Nonempty b)
    | CollapsedBlock a (AtLeastOneOf (CursorBlock a b) b)


getNodeCursorBlock block =
    case block of
        TerminalBlock a _ ->
            a

        CollapsedBlock a bs ->
            a


updateNodeCursorBlock f block =
    case block of
        TerminalBlock a bs ->
            TerminalBlock (f a) bs

        CollapsedBlock a bs ->
            CollapsedBlock (f a) bs


mapCursorBlock : (a -> c) -> (b -> d) -> CursorBlock a b -> CursorBlock c d
mapCursorBlock f g block =
    case block of
        TerminalBlock a bs ->
            TerminalBlock (f a) (Nonempty.map g bs)

        CollapsedBlock a bs ->
            CollapsedBlock (f a) (AtLeastOneOf.map (mapCursorBlock f g) g bs)


type Ctx a b
    = Top
    | Down a (List (Collapsable a b)) (List (Collapsable a b)) (Ctx a b)


mapCtx f g ctx =
    case ctx of
        Top ->
            Top

        Down a before after parctx ->
            Down (f a)
                (List.map (mapCollapsable f g) before)
                (List.map (mapCollapsable f g) after)
                (mapCtx f g parctx)


type BlockZipper a b
    = BlockZipper (Block a b) (Ctx a b)


getNodeBlockZipper (BlockZipper block ctx) =
    getNodeBlock block


type CursorZipper a b
    = CursorZipper (CursorBlock a b) (Ctx a b)


mapCursorZipper : (a -> c) -> (b -> d) -> CursorZipper a b -> CursorZipper c d
mapCursorZipper f g (CursorZipper block ctx) =
    CursorZipper (mapCursorBlock f g block) (mapCtx f g ctx)


getNodeCursorZipper (CursorZipper block ctx) =
    getNodeCursorBlock block


updateNodeCursorZipper f (CursorZipper block ctx) =
    CursorZipper (updateNodeCursorBlock f block) ctx



-- FROM TRANSLATED BLOCKS


fullyExpanded : TranslatedBlock -> Collapsable String Word
fullyExpanded block =
    case block of
        L2Word w ->
            LoneWord w

        TranslatedBlock bs_ tr_ ->
            let
                fullyExpandedBlock ( bs, tr ) =
                    case
                        AtLeastOneOf.fromNonempty
                            (Nonempty.map translatedBlockToEither bs)
                    of
                        Right words ->
                            CursorBlock <| TerminalBlock tr words

                        Left z ->
                            ExpandedBlock tr
                                (AtLeastOneOf.map fullyExpandedBlock identity z)
            in
                Block <| fullyExpandedBlock ( bs_, tr_ )


fullyCollapsed : TranslatedBlock -> Collapsable String Word
fullyCollapsed block =
    case block of
        L2Word w ->
            LoneWord w

        TranslatedBlock bs tr ->
            let
                fullyCollapsedInner ( bs_, tr_ ) =
                    case
                        AtLeastOneOf.fromNonempty
                            (Nonempty.map translatedBlockToEither bs_)
                    of
                        Right words ->
                            TerminalBlock tr_ words

                        Left bs ->
                            CollapsedBlock tr_
                                (AtLeastOneOf.map
                                    fullyCollapsedInner
                                    identity
                                    bs
                                )
            in
                -- use result of terminal block instead
                Block <| CursorBlock <| fullyCollapsedInner ( bs, tr )


translatedBlockToEither b =
    case b of
        L2Word w ->
            Right w

        TranslatedBlock bs tr ->
            Left ( bs, tr )


fullyCollapse : Collapsable a b -> Either (CursorBlock a b) b
fullyCollapse collapsable =
    case collapsable of
        LoneWord b ->
            Right b

        Block block ->
            let
                fullyCollapseBlock block =
                    case block of
                        ExpandedBlock a bs ->
                            CollapsedBlock a
                                (AtLeastOneOf.map
                                    fullyCollapseBlock
                                    identity
                                    bs
                                )

                        CursorBlock cblock ->
                            cblock
            in
                Left <| fullyCollapseBlock block



-- CURSOR ZIPPER


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



-- BLOCK ZIPPERS


initTop : Block a b -> BlockZipper a b
initTop =
    flip BlockZipper Top


top : BlockZipper a b -> BlockZipper a b
top =
    untilNothing up


up : BlockZipper a b -> Maybe (BlockZipper a b)
up (BlockZipper focus ctx) =
    let
        fromCollapsable collapsable =
            case collapsable of
                Block block ->
                    Left block

                LoneWord w ->
                    Right w
    in
        case ctx of
            Top ->
                Nothing

            Down tr before after parctx ->
                Just <|
                    BlockZipper
                        (ExpandedBlock tr
                            (case
                                AtLeastOneOf.fromList
                                    (List.map fromCollapsable before)
                             of
                                Right ws ->
                                    AtLeastOneOf ws
                                        focus
                                        (List.map fromCollapsable after)

                                Left (AtLeastOneOf before_ b_ after_) ->
                                    AtLeastOneOf before_
                                        b_
                                        (after_
                                            ++ [ Left focus ]
                                            ++ (List.map fromCollapsable after)
                                        )
                            )
                        )
                        parctx


bottom : BlockZipper a b -> CursorZipper a b
bottom (BlockZipper focus ctx) =
    case focus of
        ExpandedBlock tr (AtLeastOneOf before block after) ->
            bottom <|
                BlockZipper block
                    (Down tr
                        (List.map LoneWord before)
                        (List.map (Either.fromEither Block LoneWord) after)
                        ctx
                    )

        CursorBlock cursor ->
            CursorZipper cursor ctx


left : BlockZipper a b -> Maybe (BlockZipper a b)
left (BlockZipper focus ctx) =
    case ctx of
        Top ->
            Nothing

        Down tr before after parctx ->
            let
                prevBlock revbefore_ focus_ after_ =
                    case revbefore_ of
                        [] ->
                            Nothing

                        x :: revxs ->
                            case x of
                                Block block ->
                                    Just <|
                                        BlockZipper block
                                            (Down
                                                tr
                                                (List.reverse revxs)
                                                (focus_ :: after_)
                                                parctx
                                            )

                                _ ->
                                    prevBlock
                                        revxs
                                        x
                                        (focus_ :: after_)
            in
                prevBlock (List.reverse before) (Block focus) after


right : BlockZipper a b -> Maybe (BlockZipper a b)
right (BlockZipper focus ctx) =
    case ctx of
        Top ->
            Nothing

        Down tr before after parctx ->
            let
                nextBlock before_ focus_ after_ =
                    case after_ of
                        [] ->
                            Nothing

                        x :: xs ->
                            case x of
                                Block block ->
                                    Just <|
                                        BlockZipper block
                                            (Down
                                                tr
                                                (before_ ++ [ focus_ ])
                                                xs
                                                parctx
                                            )

                                _ ->
                                    nextBlock (before_ ++ [ focus_ ]) x xs
            in
                nextBlock before (Block focus) after


upTilRightExists : BlockZipper a b -> Maybe (BlockZipper a b)
upTilRightExists =
    let
        go tz =
            case right tz of
                Nothing ->
                    up tz |> Maybe.andThen go

                x ->
                    x
    in
        go


upTilLeftExists : BlockZipper a b -> Maybe (BlockZipper a b)
upTilLeftExists =
    let
        go tz =
            case left tz of
                Nothing ->
                    up tz |> Maybe.andThen go

                x ->
                    x
    in
        go



-- HELPERS


untilNothing : (a -> Maybe a) -> a -> a
untilNothing f a0 =
    let
        go a =
            f a |> Maybe.map go |> Maybe.withDefault a
    in
        go a0


prependToNonempty : List a -> Nonempty a -> Nonempty a
prependToNonempty xs (Nonempty y ys) =
    case xs of
        [] ->
            Nonempty y ys

        x :: xs ->
            Nonempty x (xs ++ [ y ] ++ ys)


appendToNonempty : List a -> Nonempty a -> Nonempty a
appendToNonempty xs (Nonempty y ys) =
    Nonempty y (ys ++ xs)
