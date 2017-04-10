module Helper exposing (..)


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



-- SCRAPS


testurls =
    [ "https://upload.wikimedia.org/wikipedia/commons/4/4f/hu-ad%c3%b3.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/d/d3/hu-adni.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/f/fe/hu-adekv%c3%a1t.ogg"
    ]
