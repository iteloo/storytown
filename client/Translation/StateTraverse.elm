module Translation.StateTraverse exposing (..)

import Translation.Base exposing (..)
import Helper.State exposing (..)
import Helper
import Either
import AtLeastOneOf exposing (AtLeastOneOf(..))
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


traverseAtLeastOneOf :
    AtLeastOneOf (State s a) (State s b)
    -> State s (AtLeastOneOf a b)
traverseAtLeastOneOf =
    AtLeastOneOf.foldr
        (map3 AtLeastOneOf)
        (map2 (::))
        (noMutate [])
        (map2 (::))
        (noMutate [])
        (map Either.Left)
        (map Either.Right)


traverseNonempty : Nonempty (State s a) -> State s (Nonempty a)
traverseNonempty =
    Helper.nonemptyfoldr (map2 Nonempty) (map2 (::)) (noMutate [])


{-| s ~ List c -> Maybe ( Collapsable a b, List c )
    t ~ List c -> Maybe ( Block a b , List c )
    u ~ List c -> Maybe ( CursorBlock a b , List c )
-}
traverseCollapsable :
    Collapsable (State s a) (State s b)
    -> State s (Collapsable a b)
traverseCollapsable =
    let
        leaf =
            map LoneWord

        expandedBlock a =
            map2 ExpandedBlock a
                << traverseAtLeastOneOf

        terminalBlock a =
            map2 TerminalBlock a
                << traverseNonempty

        collapsedBlock a =
            map2 CollapsedBlock a
                << traverseAtLeastOneOf
    in
        foldr
            leaf
            (map Block)
            (map CursorBlock)
            expandedBlock
            terminalBlock
            collapsedBlock


traverseLeaf : Collapsable a (State s b) -> State s (Collapsable a b)
traverseLeaf =
    traverseCollapsable << mapCollapsable noMutate identity
