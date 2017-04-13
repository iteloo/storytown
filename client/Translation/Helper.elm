module Translation.Helper exposing (..)

import Translation.Base exposing (..)
import Translation.StateTraverse
import Helper.State as State exposing (State)
import List.Nonempty as Nonempty exposing ((:::))


type alias Measured a =
    { content : a
    , width : Float
    , isEnd : Bool
    }


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


markLinebreaks :
    List { width : Float, top : Float }
    -> List { width : Float, isEnd : Bool }
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


foldLeaves :
    List c
    -> (b -> c -> d)
    -> Collapsable a b
    -> Maybe (Collapsable a d)
foldLeaves cs f col =
    let
        word : b -> State (List c) d
        word b =
            pop |> State.andThen (f b >> State.noMutate)
    in
        Translation.StateTraverse.traverseLeaf
            (mapCollapsable identity word col)
            |> State.runState cs
            |> Maybe.andThen
                (\( col, rem ) ->
                    if List.isEmpty rem then
                        Just col
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