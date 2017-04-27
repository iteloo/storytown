module Language exposing (..)


type Language
    = English
    | Mandarin


encodeLanguage : Language -> String
encodeLanguage =
    toString


decodeLanguage : String -> Maybe Language
decodeLanguage str =
    case str of
        "English" ->
            Just English

        "Mandarin" ->
            Just Mandarin

        _ ->
            Nothing


allLangs : List Language
allLangs =
    [ English, Mandarin ]
