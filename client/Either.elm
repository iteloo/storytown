module Either exposing (..)


type Either a b
    = Left a
    | Right b


mapLeft : (a -> c) -> Either a b -> Either c b
mapLeft f =
    fromEither (f >> Left) Right


mapRight : (b -> c) -> Either a b -> Either a c
mapRight f =
    fromEither Left (f >> Right)


fromEither : (a -> c) -> (b -> c) -> Either a b -> c
fromEither f g e =
    case e of
        Left a ->
            f a

        Right b ->
            g b


andRight : (b -> Either a c) -> Either a b -> Either a c
andRight =
    fromEither Left


andLeft : (a -> Either c b) -> Either a b -> Either c b
andLeft =
    flip fromEither Right
