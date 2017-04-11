module Translation.Block exposing (..)

import Translation.Base exposing (..)
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Helper
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


initTop : Block a b -> BlockZipper a b
initTop =
    flip BlockZipper Top


top : BlockZipper a b -> BlockZipper a b
top =
    Helper.untilNothing up


up : BlockZipper a b -> Maybe (BlockZipper a b)
up (BlockZipper focus ctx) =
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
