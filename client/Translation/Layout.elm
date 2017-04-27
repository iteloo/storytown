module Translation.Layout
    exposing
        ( Paragraph
        , RegisteredParagraph
        , ParagraphLayout
        , Layout(..)
        , ParagraphLayoutError(..)
        , Measurement
        , Measured
        , Measure(..)
        , toDivId
        , fromDivId
        , markLeaves
        )

import Translation.Base exposing (..)
import Translation.Path exposing (FullPath)
import Translation.StateTraverse
import Helper.State as State exposing (State)
import Either exposing (Either)
import Dict
import List.Nonempty as Nonempty exposing ((:::))
import Combine exposing (string, (<*>), (<*), (*>), (<$), (<$>), many)
import Combine.Num exposing (int)


type alias Paragraph a b =
    -- [note] we use a dictionary to sync with audio
    -- [note] be careful if we end up using this with delete and add buttons
    Dict.Dict Int
        { collapsable : Collapsable a b
        , audioUrl : Maybe String
        }


type alias RegisteredParagraph a b =
    Paragraph ( a, Maybe (CursorZipper a b) ) b


type alias ParagraphLayout =
    -- [todo] clean up namespace
    Layout ( Either (Paragraph (Either (List String) (List (Measured String))) Word) (Paragraph (Either (List String) (List (Measured String))) (Measured Word)), Either String (Measured String) )
        { paragraph :
            ( RegisteredParagraph (List (Measured String)) (Measured Word), Measured String )
        , hover : Maybe FullPath
        }
        ParagraphLayoutError


type Layout a b e
    = Measuring a
    | Formatted b
    | LayoutError e


type ParagraphLayoutError
    = CannotZipWidths


type alias Measurement =
    List { top : Float, width : Float }


type alias Measured a =
    { content : a
    , width : Float
    , isEnd : Bool
    }


type Measure
    = TransMeasure FullPath
    | WordsMeasure
    | EllipsesMeasure


transMeasureDiv : String
transMeasureDiv =
    "transMeasureDiv"


wordsMeasureDiv : String
wordsMeasureDiv =
    "wordsMeasureDiv"


ellipsesMeasureDiv : String
ellipsesMeasureDiv =
    "ellipsesMeasureDiv"


toDivId : Measure -> String
toDivId m =
    case m of
        TransMeasure ( idx, path ) ->
            String.concat
                [ transMeasureDiv
                , toString idx
                , "-"
                , path
                    |> List.map toString
                    >> List.intersperse "x"
                    >> List.foldr (++) ""
                ]

        WordsMeasure ->
            wordsMeasureDiv

        EllipsesMeasure ->
            ellipsesMeasureDiv


fromDivId : String -> Maybe Measure
fromDivId =
    Combine.parse
        (Combine.choice
            [ TransMeasure
                <$> ((,)
                        <$ string transMeasureDiv
                        <*> int
                        <* string "-"
                        <*> ((::) <$> int <*> many (string "x" *> int))
                    )
            , WordsMeasure
                <$ string wordsMeasureDiv
            , EllipsesMeasure
                <$ string ellipsesMeasureDiv
            ]
            <* Combine.end
        )
        >> Result.toMaybe
        >> Maybe.map (\( _, _, r ) -> r)


markLeaves :
    Measurement
    -> Paragraph a b
    -> Maybe (Paragraph a (Measured b))
markLeaves measurement =
    zipLeaves
        (markLinebreaks measurement)
        (\w { width, isEnd } ->
            { content = w
            , width = width
            , isEnd = isEnd
            }
        )


markLinebreaks : Measurement -> List { width : Float, isEnd : Bool }
markLinebreaks =
    let
        mkEnd width =
            { width = width, isEnd = True }
    in
        List.foldr
            (\sp mmarked ->
                Just <|
                    case mmarked of
                        Nothing ->
                            ( Nonempty.fromElement (mkEnd sp.width), sp.top )

                        Just ( marked, top ) ->
                            if sp.top == top then
                                ( { width = sp.width, isEnd = False } ::: marked
                                , top
                                )
                            else
                                ( mkEnd sp.width ::: marked, sp.top )
            )
            Nothing
            >> Maybe.map (Tuple.first >> Nonempty.toList)
            >> Maybe.withDefault []


zipLeaves :
    List c
    -> (b -> c -> d)
    -> Paragraph a b
    -> Maybe (Paragraph a d)
zipLeaves cs f =
    let
        word : b -> State (List c) d
        word b =
            pop |> State.andThen (f b >> State.noMutate)
    in
        Dict.map
            (\_ i ->
                i.collapsable
                    |> mapCollapsable identity word
                    |> Translation.StateTraverse.traverseLeaf
                    |> State.map (\col -> { i | collapsable = col })
            )
            >> Dict.foldr (State.map2 << Dict.insert)
                (State.noMutate Dict.empty)
            >> State.runState cs
            >> Maybe.andThen
                (\( para, rem ) ->
                    if List.isEmpty rem then
                        Just para
                    else
                        Nothing
                )


pop : State (List a) a
pop =
    State.get
        |> State.andThen
            (\cs ->
                case cs of
                    [] ->
                        State.fail

                    c :: rem ->
                        State.put rem
                            |> State.discardAndThen
                                (State.noMutate c)
            )


{-| [note] unused
-}
splitLines : List { c | top : b, width : a } -> List ( List a, b )
splitLines =
    List.foldr
        (\sp lines ->
            case lines of
                [] ->
                    [ ( [ sp.width ], sp.top ) ]

                ( sps, top ) :: rest ->
                    if sp.top == top then
                        ( sp.width :: sps, top ) :: rest
                    else
                        ( [ sp.width ], sp.top ) :: lines
        )
        []
