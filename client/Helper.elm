module Helper exposing (..)


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe =
    List.foldr (\m -> Maybe.andThen (\l -> Maybe.map (flip (::) l) m))
        (Just [])



-- SCRAPS


testurls =
    [ "https://upload.wikimedia.org/wikipedia/commons/4/4f/hu-ad%c3%b3.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/d/d3/hu-adni.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/f/fe/hu-adekv%c3%a1t.ogg"
    ]
