module AtLeastOneOf exposing (..)

import Either exposing (Either(..))
import List.Nonempty as Nonempty exposing (Nonempty(..))


type AtLeastOneOf a b
    = AtLeastOneOf (List b) a (List (Either a b))


toList : AtLeastOneOf a a -> List a
toList (AtLeastOneOf before focus after) =
    before ++ [ focus ] ++ List.map (Either.fromEither identity identity) after


fromNonempty : Nonempty (Either a b) -> Either (AtLeastOneOf a b) (Nonempty b)
fromNonempty (Nonempty b1 bs) =
    List.foldl
        (\ab ebs ->
            case ebs of
                Right bs ->
                    ab
                        |> Either.mapRight
                            (Nonempty.append bs << Nonempty.fromElement)
                        |> Either.mapLeft
                            (\a -> AtLeastOneOf (Nonempty.toList bs) a [])

                Left (AtLeastOneOf before a after) ->
                    Left (AtLeastOneOf before a (after ++ [ ab ]))
        )
        (b1
            |> Either.mapRight Nonempty.fromElement
            |> Either.mapLeft (\a -> AtLeastOneOf [] a [])
        )
        bs


fromList : List (Either a b) -> Either (AtLeastOneOf a b) (List b)
fromList =
    List.foldl
        (\ab ebs ->
            case ebs of
                Right bs ->
                    ab
                        |> Either.mapRight
                            (\b -> bs ++ [ b ])
                        |> Either.mapLeft
                            (\a -> AtLeastOneOf bs a [])

                Left (AtLeastOneOf before a after) ->
                    Left (AtLeastOneOf before a (after ++ [ ab ]))
        )
        (Right [])


map : (a -> c) -> (b -> d) -> AtLeastOneOf a b -> AtLeastOneOf c d
map f g (AtLeastOneOf before focus after) =
    AtLeastOneOf (List.map g before)
        (f focus)
        (List.map (Either.mapLeft f >> Either.mapRight g) after)
