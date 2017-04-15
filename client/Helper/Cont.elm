module Helper.Cont exposing (..)


type alias Cont r a =
    (a -> r) -> r


map : (a -> b) -> Cont r a -> Cont r b
map f ma k =
    ma (f >> k)


pure : a -> Cont r a
pure a =
    (|>) a


andThen : (a -> Cont r b) -> Cont r a -> Cont r b
andThen f ma k =
    ma (flip f k)


(<*>) : Cont r (a -> b) -> Cont r a -> Cont r b
(<*>) mf ma k =
    mf (\f -> map f ma k)


(<<|) : (a -> b) -> Cont r a -> Cont r b
(<<|) =
    (<*>) << pure


map2 : (a -> b -> c) -> Cont r a -> Cont r b -> Cont r c
map2 f ma mb =
    pure f <*> ma <*> mb


map3 : (a -> a1 -> a2 -> b) -> Cont r a -> Cont r a1 -> Cont r a2 -> Cont r b
map3 f ma mb mc =
    pure f <*> ma <*> mb <*> mc
