module Helper exposing (..)

import Helper.Cont as Cont exposing (Cont)
import Regex
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


isJust : Maybe a -> Bool
isJust =
    not << isNothing


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        Just _ ->
            False


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


nonemptyFoldrFull : (a -> s -> s) -> (a -> t -> s) -> t -> Nonempty a -> s
nonemptyFoldrFull scons revNonempty tnil xs =
    case Nonempty.reverse xs of
        Nonempty last xs ->
            List.foldr scons (revNonempty last tnil) (List.reverse xs)


nonemptyTraverseCont : Nonempty (Cont r a) -> Cont r (Nonempty a)
nonemptyTraverseCont =
    nonemptyfoldr
        (Cont.map2 Nonempty)
        (Cont.map2 (::))
        (Cont.pure [])


nonemptyLast : Nonempty a -> a
nonemptyLast =
    Nonempty.head << Nonempty.reverse


truncateListAfter : (a -> Bool) -> List a -> List (Nonempty a)
truncateListAfter cond =
    List.foldr (mkCons cond (::)) []


truncateAfter : (a -> Bool) -> Nonempty a -> Nonempty (Nonempty a)
truncateAfter cond =
    nonemptyfoldr (mkCons cond Nonempty) (mkCons cond (::)) []


mkCons :
    (a -> Bool)
    -> (Nonempty a -> List (Nonempty a) -> s)
    -> a
    -> List (Nonempty a)
    -> s
mkCons cond cons a segs =
    if cond a then
        cons (Nonempty.fromElement a) segs
    else
        case segs of
            [] ->
                cons (Nonempty.fromElement a) []

            x :: xs ->
                cons (a ::: x) xs


zipList : List a -> List b -> Maybe (List ( a, b ))
zipList xs ys =
    case ( xs, ys ) of
        ( x :: xs, y :: ys ) ->
            Maybe.map ((::) ( x, y )) (zipList xs ys)

        ( [], [] ) ->
            Just []

        _ ->
            Nothing


splitByAndPreserveSpaces : String -> List String
splitByAndPreserveSpaces =
    Regex.find Regex.All (Regex.regex "[^\\s]*\\s*")
        >> List.map .match



-- SCRAPS


testurls : List String
testurls =
    [ "https://upload.wikimedia.org/wikipedia/commons/4/4f/hu-ad%c3%b3.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/d/d3/hu-adni.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/f/fe/hu-adekv%c3%a1t.ogg"
    ]
