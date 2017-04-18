module Translation.Path exposing (..)

import Translation.Base exposing (..)
import Translation.Block as Block
import Helper.State2 exposing (..)
import Either
import AtLeastOneOf exposing (AtLeastOneOf(..))
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))
import Helper


type alias Path =
    List Int


nodeAtPath : Path -> Collapsable a b -> Maybe a
nodeAtPath path col =
    case col of
        LoneLeaf _ ->
            Nothing

        Block b ->
            path
                |> List.map (flip List.repeat Block.right)
                |> List.intersperse [ Block.down ]
                |> List.concat
                |> List.foldl Maybe.andThen (Just <| Block.initTop b)
                |> Maybe.map getNodeBlockZipper


setAtPath : Path -> a -> Collapsable a b -> Maybe (Collapsable a b)
setAtPath path a col =
    case col of
        LoneLeaf _ ->
            Nothing

        Block b ->
            path
                |> List.map (flip List.repeat Block.right)
                |> List.intersperse [ Block.down ]
                |> List.concat
                |> List.foldl Maybe.andThen (Just <| Block.initTop b)
                |> Maybe.map
                    (updateNodeBlockZipper (always a)
                        >> Block.top
                        >> Block.underlyingCollapsable
                    )


pathedMap : (Path -> a -> c) -> Collapsable a b -> Collapsable c b
pathedMap f =
    mapCollapsable
        (\a ->
            get
                |> andThen
                    (\( idxs, idx ) ->
                        mutate (Tuple.mapSecond ((+) 1))
                            |> discardAndThen
                                (noMutate (f (idxs ++ [ idx ]) a))
                    )
        )
        noMutate
        >> foldr
            (map LoneLeaf)
            (map Block)
            (map CursorBlock)
            (\sa sbs ->
                get
                    |> andThen
                        (\( idxs, idx ) ->
                            traverseAtLeastOneOf sbs
                                |> runState ( idxs ++ [ idx ], 0 )
                                |> Tuple.first
                                |> flip ExpandedBlock
                                |> map
                                |> (\f -> f sa)
                        )
            )
            (\sa sbs ->
                get
                    |> andThen
                        (\( idxs, idx ) ->
                            traverseNonempty sbs
                                |> runState ( idxs ++ [ idx ], 0 )
                                |> Tuple.first
                                |> flip TerminalBlock
                                |> map
                                |> (\f -> f sa)
                        )
            )
            (\sa sbs ->
                get
                    |> andThen
                        (\( idxs, idx ) ->
                            traverseAtLeastOneOf sbs
                                |> runState ( idxs ++ [ idx ], 0 )
                                |> Tuple.first
                                |> flip CollapsedBlock
                                |> map
                                |> (\f -> f sa)
                        )
            )
        >> runState ( [], 0 )
        >> Tuple.first


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
