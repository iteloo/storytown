module AtLeastOneOf exposing (..)

import Either exposing (Either(..))
import Helper.Cont as Cont exposing (..)
import List.Nonempty as Nonempty exposing (Nonempty(..))


type AtLeastOneOf a b
    = AtLeastOneOf (List b) a (List (Either a b))


{-| AtLeastOneOf a b ~ s
    List b ~ t
    b ~ b
    a ~ a
    List (Either a b) u
    Either a b ~ v
-}
foldr :
    -- AtLeastOneOf
    (t -> a -> u -> s)
    -- (::)
    -> (b -> t -> t)
       -- []
    -> t
       -- (::)
    -> (v -> u -> u)
       -- []
    -> u
       -- Left
    -> (a -> v)
       -- Right
    -> (b -> v)
    -> AtLeastOneOf a b
    -> s
foldr atLeastOneOf cn1 n1 cn2 n2 l r (AtLeastOneOf bs a abs) =
    atLeastOneOf (List.foldr cn1 n1 bs)
        a
        (List.foldr cn2 n2 (List.map (Either.fromEither l r) abs))


foldl :
    (t -> a -> u -> s)
    -> (b -> t -> t)
    -> t
    -> (v -> u -> u)
    -> u
    -> (a -> v)
    -> (b -> v)
    -> AtLeastOneOf a b
    -> s
foldl atLeastOneOf cn1 n1 cn2 n2 l r abs =
    traverseHelper atLeastOneOf
        cn1
        n1
        cn2
        n2
        l
        r
        (map Cont.pure Cont.pure abs)
        identity


traverseCont : AtLeastOneOf (Cont r a) (Cont r b) -> Cont r (AtLeastOneOf a b)
traverseCont =
    traverseHelper AtLeastOneOf (::) [] (::) [] Left Right


traverseHelper :
    (t -> a -> u -> s)
    -> (b -> t -> t)
    -> t
    -> (v -> u -> u)
    -> u
    -> (a -> v)
    -> (b -> v)
    -> AtLeastOneOf (Cont r a) (Cont r b)
    -> Cont r s
traverseHelper atLeastOneOf cn1 n1 cn2 n2 l r abs =
    foldr (Cont.map3 atLeastOneOf)
        (Cont.map2 cn1)
        (Cont.pure n1)
        (Cont.map2 cn2)
        (Cont.pure n2)
        (Cont.map l)
        (Cont.map r)
        abs


toList : AtLeastOneOf a a -> List a
toList (AtLeastOneOf before focus after) =
    List.concat
        [ before
        , [ focus ]
        , List.map (Either.fromEither identity identity) after
        ]


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


{-| [note] kind of weird focus can be anywhere in `AtLeastOneOf a a`
-}
toNonempty : AtLeastOneOf a a -> Nonempty a
toNonempty (AtLeastOneOf before focus after) =
    let
        after_ =
            List.map (Either.fromEither identity identity) after
    in
        case before of
            [] ->
                Nonempty focus after_

            x :: xs ->
                Nonempty x (xs ++ [ focus ] ++ after_)


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


map : (a -> c) -> (b -> d) -> AtLeastOneOf a b -> AtLeastOneOf c d
map f g (AtLeastOneOf before focus after) =
    AtLeastOneOf (List.map g before)
        (f focus)
        (List.map (Either.mapLeft f >> Either.mapRight g) after)
