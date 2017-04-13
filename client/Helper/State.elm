module Helper.State exposing (..)


type alias State s a =
    s -> Maybe ( a, s )


mutate : (s -> Maybe s) -> State s ()
mutate f =
    get |> andThen (\s -> f s |> Maybe.map put |> Maybe.withDefault fail)


put : s -> State s ()
put s _ =
    Just ( (), s )


get : State s s
get s =
    Just ( s, s )


fail : State s a
fail _ =
    Nothing


guardState : (s -> Bool) -> State s ()
guardState cond =
    get
        |> andThen
            (\s ->
                if cond s then
                    noMutate ()
                else
                    fail
            )


runState : s -> State s a -> Maybe ( a, s )
runState =
    flip (<|)


andThen : (a -> State s b) -> State s a -> State s b
andThen f =
    (<<) (Maybe.andThen (uncurry f))


discardAndThen : State s b -> State s a -> State s b
discardAndThen =
    always >> andThen


(<<<) : (b -> State s c) -> (a -> State s b) -> a -> State s c
(<<<) f =
    (<<) (andThen f)


(>>>) : (a -> State s b) -> (b -> State s c) -> a -> State s c
(>>>) =
    flip (<<<)


noMutate : a -> State s a
noMutate =
    curry Just


map : (a -> b) -> State s a -> State s b
map f =
    (<<) (Maybe.map (Tuple.mapFirst f))


(<<|) : (a -> b) -> State s a -> State s b
(<<|) =
    (<*>) << noMutate


(<*>) : State s (a -> b) -> State s a -> State s b
(<*>) fa aa =
    fa |> andThen (\f -> aa |> andThen (\a -> noMutate (f a)))


map2 : (a -> b -> c) -> State s a -> State s b -> State s c
map2 f a b =
    f <<| a <*> b


map3 : (a -> b -> c -> d) -> State s a -> State s b -> State s c -> State s d
map3 f a b c =
    f <<| a <*> b <*> c
