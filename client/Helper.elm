module Helper exposing (..)

import Lazy
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


maybeToList : Maybe a -> List a
maybeToList =
    Maybe.map List.singleton >> Maybe.withDefault []


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe =
    List.foldr (\m -> Maybe.andThen (\l -> Maybe.map (flip (::) l) m))
        (Just [])


isOk : Result e a -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


isErr : Result e a -> Bool
isErr r =
    case r of
        Ok _ ->
            False

        Err _ ->
            True


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


nonemptyfoldr : (a -> u -> s) -> (a -> u -> u) -> u -> Nonempty a -> s
nonemptyfoldr nonempty tcons tnil (Nonempty x xs) =
    nonempty x (List.foldr tcons tnil xs)


nonemptyLast : Nonempty a -> a
nonemptyLast =
    Nonempty.head << Nonempty.reverse


truncateAfter : (a -> Bool) -> Nonempty a -> Nonempty (Nonempty a)
truncateAfter cond =
    let
        mkCons :
            (Nonempty a -> List (Nonempty a) -> s)
            -> a
            -> List (Nonempty a)
            -> s
        mkCons cons =
            (\a segs ->
                if cond a then
                    cons (Nonempty.fromElement a) segs
                else
                    case segs of
                        [] ->
                            cons (Nonempty.fromElement a) []

                        x :: xs ->
                            cons (a ::: x) xs
            )
    in
        nonemptyfoldr (mkCons Nonempty) (mkCons (::)) []



-- undefined : a
-- undefined =
--     Lazy.force <| Lazy.lazy <| \() -> Debug.crash "undefined"
-- SCRAPS


testurls =
    [ "https://upload.wikimedia.org/wikipedia/commons/4/4f/hu-ad%c3%b3.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/d/d3/hu-adni.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/f/fe/hu-adekv%c3%a1t.ogg"
    ]
