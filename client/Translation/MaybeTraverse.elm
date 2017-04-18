module Translation.MaybeTraverse exposing (..)

import Translation.Base exposing (..)
import Helper
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


traverse : Collapsable (Maybe a) (Maybe b) -> Maybe (Collapsable a b)
traverse =
    foldr
        (Maybe.map LoneLeaf)
        (Maybe.map Block)
        (Maybe.map CursorBlock)
        (\a ->
            Maybe.map2 ExpandedBlock a
                << AtLeastOneOf.foldr
                    (Maybe.map3 AtLeastOneOf)
                    (Maybe.map2 (::))
                    (Just [])
                    (Maybe.map2 (::))
                    (Just [])
                    (Maybe.map Left)
                    (Maybe.map Right)
        )
        (\a ->
            Maybe.map2 TerminalBlock a
                << Helper.nonemptyfoldr
                    (Maybe.map2 Nonempty)
                    (Maybe.map2 (::))
                    (Just [])
        )
        (\a ->
            Maybe.map2 CollapsedBlock a
                << AtLeastOneOf.foldr
                    (Maybe.map3 AtLeastOneOf)
                    (Maybe.map2 (::))
                    (Just [])
                    (Maybe.map2 (::))
                    (Just [])
                    (Maybe.map Left)
                    (Maybe.map Right)
        )
