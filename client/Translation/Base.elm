module Translation.Base exposing (..)

import Parser exposing (TranslatedBlock(..))
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf)
import List.Nonempty as Nonempty exposing (Nonempty)


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


type Zipper a b
    = Zipper (Collapsable a b) (Ctx a b)


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


type LeafZipper a b
    = LeafZipper b (Ctx a b)



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



-- HELPER


fromCollapsable collapsable =
    case collapsable of
        Block block ->
            Left block

        LoneWord w ->
            Right w
