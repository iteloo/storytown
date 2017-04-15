module Translation.Zipper exposing (..)

{-| This translation module doesn't remember the cursors for collapsed subtree,
and each collapsed subtree is fully collapsed.
-}

import Translation.Base exposing (..)
import Either exposing (Either(..))
import AtLeastOneOf exposing (AtLeastOneOf(..))
import Helper
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


initTop : Collapsable a b -> Zipper a b
initTop =
    flip Zipper Top


top : Zipper a b -> Zipper a b
top =
    Helper.untilNothing up


up : Zipper a b -> Maybe (Zipper a b)
up (Zipper focus ctx) =
    let
        fromCollapsable collapsable =
            case collapsable of
                Block block ->
                    Left block

                LoneLeaf w ->
                    Right w
    in
        case ctx of
            Top ->
                Nothing

            Down tr before after parctx ->
                Just <|
                    Zipper
                        (Block <|
                            Either.fromEither
                                (ExpandedBlock tr)
                                (CursorBlock << TerminalBlock tr)
                                (AtLeastOneOf.fromNonempty <|
                                    Nonempty.map fromCollapsable <|
                                        (Nonempty.fromElement focus
                                            |> Helper.prependToNonempty before
                                            |> Helper.appendToNonempty after
                                        )
                                )
                        )
                        parctx


left : Zipper a b -> Maybe (Zipper a b)
left (Zipper focus ctx) =
    case ctx of
        Top ->
            Nothing

        Down tr before after parctx ->
            case List.reverse before of
                [] ->
                    Nothing

                x :: revxs ->
                    Just <|
                        Zipper x
                            (Down tr
                                (List.reverse revxs)
                                (focus :: after)
                                parctx
                            )


right : Zipper a b -> Maybe (Zipper a b)
right (Zipper focus ctx) =
    case ctx of
        Top ->
            Nothing

        Down tr before after parctx ->
            case after of
                [] ->
                    Nothing

                x :: xs ->
                    Just <| Zipper x (Down tr (before ++ [ focus ]) xs parctx)


upTilRightExists : Zipper a b -> Maybe (Zipper a b)
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


upTilLeftExists : Zipper a b -> Maybe (Zipper a b)
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
