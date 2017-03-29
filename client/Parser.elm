module Parser
    exposing
        ( parseTranslatedText
        , TranslatedText
        , TranslatedBlock(..)
        )

import Combine exposing (..)
import Combine.Char exposing (..)
import List.Nonempty as NList exposing (Nonempty, (:::))



type alias TranslatedText =
    List TranslatedBlock


type TranslatedBlock
    = L2Word String
    | TranslatedBlock (Nonempty TranslatedBlock) String


parseTranslatedText input =
    case runParser translatedText (NList.fromElement 0) input of
        Ok ( _, _, result ) ->
            Ok result

        Err ( _, _, errors ) ->
            Err (String.join " or " errors)


type Expr
    = UntranslatedWord String
    | WordTranslation String String
    | BlockTranslation String (Nonempty Expr)


translatedBlockFromExpr : Expr -> TranslatedBlock
translatedBlockFromExpr exp =
    case exp of
        UntranslatedWord w ->
            L2Word w

        WordTranslation w t ->
            TranslatedBlock (NList.fromElement (L2Word w)) t

        BlockTranslation t exps ->
            TranslatedBlock (NList.map translatedBlockFromExpr exps) t


type alias ParseState =
    Nonempty Int


countSpaces =
    map String.length (regex " *")


indentation p =
    withState
        (\s ->
            countSpaces
                >>= \n ->
                        let
                            i =
                                NList.head s
                        in
                            if n == i then
                                succeed ()
                            else
                                fail ("expected " ++ toString i ++ " spaces of indentation")
        )
        *> p


indent =
    lookAhead <|
        countSpaces
            >>= (\n ->
                    withState <|
                        \s ->
                            if n > NList.head s then
                                putState (n ::: s)
                            else
                                fail "expected indentation"
                )


unindent =
    lookAhead <|
        countSpaces
            >>= \n ->
                    withState <|
                        \s ->
                            if NList.head s >= n then
                                putState (NList.pop s)
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
            *> (unsafe << NList.fromList <$> many1 (indentation expr))
            <* unindent


expr : Parser ParseState Expr
expr =
    lazy <|
        \_ ->
            (BlockTranslation <$> line <*> block)
                <|> (WordTranslation
                        <$> map String.fromList (manyTill notEol (char '/'))
                        <*> line
                    )
                <|> (UntranslatedWord <$> line)


exprs : Parser ParseState (List Expr)
exprs =
    manyTill expr end


notEol =
    noneOf [ '\x0D', '\n' ]


line =
    map String.fromList (manyTill anyChar eol)


translatedText =
    map (List.map translatedBlockFromExpr) exprs



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
