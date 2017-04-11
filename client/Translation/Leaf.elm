module Translation.Leaf exposing (..)

import Translation.Base exposing (..)
import Translation.Zipper as Zipper exposing (..)
import Translation.Block as Block
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Helper
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


moveToLeaf : Zipper a b -> LeafZipper a b
moveToLeaf (Zipper focus ctx) =
    case focus of
        LoneWord b ->
            LeafZipper b ctx

        Block block ->
            moveToLeaf <|
                case block of
                    ExpandedBlock a (AtLeastOneOf before block after) ->
                        case before of
                            [] ->
                                Zipper (Block block)
                                    (Down a
                                        []
                                        (List.map
                                            (Either.fromEither
                                                Block
                                                LoneWord
                                            )
                                            after
                                        )
                                        ctx
                                    )

                            b :: bs ->
                                Zipper (LoneWord b)
                                    (Down a
                                        []
                                        (List.concat
                                            [ List.map LoneWord bs
                                            , [ Block block ]
                                            , List.map
                                                (Either.fromEither
                                                    Block
                                                    LoneWord
                                                )
                                                after
                                            ]
                                        )
                                        ctx
                                    )

                    CursorBlock block ->
                        case block of
                            TerminalBlock a (Nonempty b bs) ->
                                Zipper (LoneWord b)
                                    (Down a [] (List.map LoneWord bs) ctx)

                            CollapsedBlock a (AtLeastOneOf before block after) ->
                                case before of
                                    [] ->
                                        Zipper
                                            (Block <| CursorBlock block)
                                            (Down a
                                                []
                                                (List.map
                                                    (Either.fromEither
                                                        (Block << CursorBlock)
                                                        LoneWord
                                                    )
                                                    after
                                                )
                                                ctx
                                            )

                                    b :: bs ->
                                        Zipper (LoneWord b)
                                            (Down a
                                                []
                                                (List.concat
                                                    [ List.map LoneWord bs
                                                    , [ Block <| CursorBlock block ]
                                                    , List.map
                                                        (Either.fromEither
                                                            (Block << CursorBlock)
                                                            LoneWord
                                                        )
                                                        after
                                                    ]
                                                )
                                                ctx
                                            )


toZipper : LeafZipper a b -> Zipper a b
toZipper (LeafZipper focus ctx) =
    Zipper (LoneWord focus) ctx


right : LeafZipper a b -> Maybe (LeafZipper a b)
right =
    toZipper >> Zipper.right >> Maybe.map moveToLeaf
