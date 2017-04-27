module Parser
    exposing
        ( parseTranslatedText
        , TranslatedText
        , TranslatedBlock(..)
        )

import Language exposing (Language(..))
import Helper
import Combine exposing (..)
import Combine.Char exposing (..)
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


type alias TranslatedText =
    List TranslatedBlock


type TranslatedBlock
    = L2Word String
    | TranslatedBlock (Nonempty TranslatedBlock) String


parseTranslatedText : Language -> String -> Result String TranslatedBlock
parseTranslatedText lang input =
    case runParser (translatedText lang) (Nonempty.fromElement 0) input of
        Ok ( _, _, result ) ->
            Ok result

        Err ( _, _, errors ) ->
            Err (String.join " or " errors)


type Expr
    = UntranslatedWord String
    | WordTranslation String String
    | BlockTranslation String (Nonempty Expr)


translatedBlockFromExpr : Language -> Expr -> TranslatedBlock
translatedBlockFromExpr lang exp =
    case exp of
        UntranslatedWord w ->
            L2Word w

        WordTranslation w t ->
            TranslatedBlock
                (case lang of
                    English ->
                        case
                            w
                                |> Helper.splitByAndPreserveSpaces
                                |> List.map L2Word
                                |> Nonempty.fromList
                        of
                            Nothing ->
                                Debug.crash "regex should have at least 1 match"

                            Just ws ->
                                ws

                    Mandarin ->
                        w
                            |> String.toList
                            |> List.map String.fromChar
                            |> Nonempty.fromList
                            |> Maybe.withDefault (Nonempty.fromElement w)
                            |> Nonempty.map L2Word
                )
                t

        BlockTranslation t exps ->
            TranslatedBlock (Nonempty.map (translatedBlockFromExpr lang) exps) t


type alias ParseState =
    Nonempty Int


countSpaces : Parser s Int
countSpaces =
    map String.length (regex " *")


indentation : Parser ParseState a -> Parser ParseState a
indentation p =
    withState
        (\s ->
            countSpaces
                >>= \n ->
                        let
                            i =
                                Nonempty.head s
                        in
                            if n == i then
                                succeed ()
                            else
                                fail
                                    ("expected "
                                        ++ toString i
                                        ++ " spaces of indentation"
                                    )
        )
        *> p


indent : Parser ParseState ()
indent =
    lookAhead <|
        countSpaces
            >>= (\n ->
                    withState <|
                        \s ->
                            if n > Nonempty.head s then
                                putState (n ::: s)
                            else
                                fail "expected indentation"
                )


unindent : Parser ParseState ()
unindent =
    lookAhead <|
        countSpaces
            >>= \n ->
                    withState <|
                        \s ->
                            if Nonempty.head s >= n then
                                putState (Nonempty.pop s)
                            else
                                fail "expect unindentation"


block : Parser ParseState (Nonempty Expr)
block =
    let
        unsafe m =
            case m of
                Nothing ->
                    Debug.crash "this should never happen"

                Just x ->
                    x
    in
        indent
            *> (unsafe << Nonempty.fromList <$> many1 (indentation expr))
            <* unindent


expr : Parser ParseState Expr
expr =
    lazy <|
        \_ ->
            (BlockTranslation <$> line <*> block)
                <|> (WordTranslation
                        <$> map (String.fromList << Nonempty.toList)
                                (manyTill1 notEol (char '/'))
                        <*> line
                    )
                <|> (UntranslatedWord <$> line)


manyTill1 : Parser s a -> Parser s end -> Parser s (Nonempty a)
manyTill1 p delim =
    (delim *> fail "nothing before delim") <|> (Nonempty <$> p <*> manyTill p delim)


{-| [unused]
-}
exprs : Parser ParseState (List Expr)
exprs =
    manyTill expr end


notEol : Parser s Char
notEol =
    noneOf [ '\x0D', '\n' ]


line : Parser s String
line =
    -- [todo] can be made more efficient
    String.fromList <$> (manyTill anyChar eol <|> manyTill anyChar end)


translatedText : Language -> Parser ParseState TranslatedBlock
translatedText lang =
    translatedBlockFromExpr lang <$> expr <* end



-- HELPERS


dropWhile : (a -> Bool) -> List a -> List a
dropWhile p xs =
    case xs of
        [] ->
            []

        x :: ys ->
            if p x then
                dropWhile p ys
            else
                xs
