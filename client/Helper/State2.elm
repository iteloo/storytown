module Helper.State2 exposing (..)


type alias State s a =
    s -> ( a, s )


mutate : (s -> s) -> State s ()
mutate f =
    get |> andThen (\s -> put (f s))


put : s -> State s ()
put s _ =
    ( (), s )


get : State s s
get s =
    ( s, s )


runState : s -> State s a -> ( a, s )
runState =
    flip (<|)


andThen : (a -> State s b) -> State s a -> State s b
andThen f =
    (<<) (uncurry f)


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
    (,)


map : (a -> b) -> State s a -> State s b
map f =
    (<<) (Tuple.mapFirst f)


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
