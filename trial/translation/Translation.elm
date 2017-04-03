module Translation exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))
import Parser exposing (TranslatedBlock(..))
import Html exposing (..)
import Html.Attributes exposing (..)


trans =
    TranslatedBlock
        (Nonempty
            (TranslatedBlock
                (Nonempty (L2Word "what!") [])
                "什麼！"
            )
            [ (TranslatedBlock
                (Nonempty
                    (L2Word "he")
                    [ TranslatedBlock
                        (Nonempty (L2Word "is") [ L2Word "alive" ])
                        "活著"
                    ]
                )
                "他活著"
              )
            ]
        )
        "什麼！他還活著！？"


test : CursorZipper String Word
test =
    case trans |> fullyCollapsed |> initCursorZipper of
        Nothing ->
            Debug.crash "yeah right"

        Just a ->
            a


testMain =
    text <|
        toString <|
            (trans
                |> fullyCollapsed
                |> Debug.log "fullyCollapsed"
                -- |> fullyExpanded
                -- |> Debug.log "fullyExpanded"
                |>
                    initCursorZipper
                |> Maybe.andThen expand
                |> Debug.log "expand"
                |> Debug.log "init"
                |> Maybe.map
                    (mapCursorZipper (\str -> ( str, Nothing )) identity
                        >> register
                        >> untilNothing
                            (rightBottom >> Maybe.map register)
                        >> untilNothing
                            (leftBottom >> Maybe.map register)
                        >> toBlockZipper
                        >> top
                    )
             -- |> Maybe.andThen collapse
             -- |> Debug.log "collapse"
             -- |>
             --     Maybe.andThen rightBottom
             -- |> Debug.log "rightBottom"
             -- |> Maybe.andThen expand
             -- |> Debug.log "expand"
             -- |> Maybe.andThen collapse
             -- |> Debug.log "collapse"
             -- |> Maybe.andThen rightBottom
             -- |> Debug.log "rightBottom"
             -- |> Maybe.andThen rightBottom
             -- |> Debug.log "rightBottom"
             -- |> Maybe.andThen rightBottom
             -- |> Debug.log "rightBottom"
            )


toList : EitherZipper a a -> List a
toList (EitherZipper before focus after) =
    before ++ [ focus ] ++ List.map (fromEither identity identity) after


register :
    CursorZipper ( a, Maybe (CursorZipper a b) ) b
    -> CursorZipper ( a, Maybe (CursorZipper a b) ) b
register z =
    updateNodeCursorZipper
        (\( a, _ ) ->
            ( a, Just (mapCursorZipper (\( a, _ ) -> a) identity z) )
        )
        z


untilNothing : (a -> Maybe a) -> a -> a
untilNothing f a0 =
    let
        go a =
            f a |> Maybe.map go |> Maybe.withDefault a
    in
        go a0


type alias Word =
    String


type Collapsable a b
    = LoneWord b
    | Block (Block a b)


mapCollapsable f g c =
    case c of
        LoneWord b ->
            LoneWord (g b)

        Block block ->
            Block (mapBlock f g block)


type Block a b
    = CursorBlock (CursorBlock a b)
    | ExpandedBlock a (EitherZipper (Block a b) b)


mapBlock : (a -> c) -> (b -> d) -> Block a b -> Block c d
mapBlock f g block =
    case block of
        CursorBlock block ->
            CursorBlock (mapCursorBlock f g block)

        ExpandedBlock a bs ->
            ExpandedBlock (f a) (mapEitherZipper (mapBlock f g) g bs)


getNodeBlock block =
    case block of
        ExpandedBlock a _ ->
            a

        CursorBlock cblock ->
            getNodeCursorBlock cblock


type CursorBlock a b
    = TerminalBlock a (Nonempty b)
    | CollapsedBlock (CursorZipper a b)


getNodeCursorBlock block =
    case block of
        TerminalBlock a _ ->
            a

        CollapsedBlock z ->
            -- [problem] need to zip back up first
            getNodeBlockZipper <| top <| toBlockZipper z


updateNodeCursorBlock f block =
    let
        -- [hack]
        updateNodeTop f (CursorZipper b ctx) =
            let
                go ctx =
                    case ctx of
                        Down a before after Top ->
                            Down (f a) before after Top

                        Down a before after parctx ->
                            Down a before after (go parctx)

                        Top ->
                            Top
            in
                CursorZipper b (go ctx)
    in
        case block of
            TerminalBlock a bs ->
                TerminalBlock (f a) bs

            CollapsedBlock z ->
                CollapsedBlock <| updateNodeTop f z


mapCursorBlock f g block =
    case block of
        TerminalBlock a bs ->
            TerminalBlock (f a) (Nonempty.map g bs)

        CollapsedBlock z ->
            CollapsedBlock (mapCursorZipper f g z)


type Ctx a b
    = Top
    | Down a (List (Collapsable a b)) (List (Collapsable a b)) (Ctx a b)


mapCtx f g ctx =
    case ctx of
        Top ->
            Top

        Down a before after parctx ->
            Down (f a)
                (List.map (mapCollapsable f g) before)
                (List.map (mapCollapsable f g) after)
                (mapCtx f g parctx)


type BlockZipper a b
    = BlockZipper (Block a b) (Ctx a b)


getNodeBlockZipper (BlockZipper block ctx) =
    getNodeBlock block


type CursorZipper a b
    = CursorZipper (CursorBlock a b) (Ctx a b)


mapCursorZipper : (a -> c) -> (b -> d) -> CursorZipper a b -> CursorZipper c d
mapCursorZipper f g (CursorZipper block ctx) =
    CursorZipper (mapCursorBlock f g block) (mapCtx f g ctx)


getNodeCursorZipper (CursorZipper block ctx) =
    getNodeCursorBlock block


updateNodeCursorZipper f (CursorZipper block ctx) =
    CursorZipper (updateNodeCursorBlock f block) ctx



-- INIT


translatedBlockToEither b =
    case b of
        L2Word w ->
            Right w

        TranslatedBlock bs tr ->
            Left ( bs, tr )


fullyExpanded : TranslatedBlock -> Collapsable String Word
fullyExpanded block =
    case block of
        L2Word w ->
            LoneWord w

        TranslatedBlock bs_ tr_ ->
            let
                fullyExpandedBlock ( bs, tr ) =
                    case
                        fromNonempty (Nonempty.map translatedBlockToEither bs)
                    of
                        Right words ->
                            CursorBlock <| TerminalBlock tr words

                        Left z ->
                            ExpandedBlock tr
                                (mapEitherZipper fullyExpandedBlock identity z)
            in
                Block <| fullyExpandedBlock ( bs_, tr_ )


fullyCollapsed : TranslatedBlock -> Collapsable String Word
fullyCollapsed block =
    case block of
        L2Word w ->
            LoneWord w

        TranslatedBlock bs tr ->
            let
                fullyCollapsedInner ( bs_, tr_ ) =
                    case
                        fromNonempty
                            (Nonempty.map translatedBlockToEither bs_)
                    of
                        Right words ->
                            TerminalBlock tr_ words

                        Left (EitherZipper before focus after) ->
                            CollapsedBlock <|
                                CursorZipper
                                    (fullyCollapsedInner focus)
                                    (Down tr_
                                        (List.map
                                            (fullyCollapsed
                                                << L2Word
                                            )
                                            before
                                        )
                                        (List.map
                                            (fullyCollapsed
                                                << fromEither
                                                    (uncurry TranslatedBlock)
                                                    L2Word
                                            )
                                            after
                                        )
                                        Top
                                    )
            in
                -- use result of terminal block instead
                Block <| CursorBlock <| fullyCollapsedInner ( bs, tr )



-- CURSOR ZIPPER


initCursorZipper : Collapsable a b -> Maybe (CursorZipper a b)
initCursorZipper collapsable =
    case collapsable of
        LoneWord _ ->
            Nothing

        Block block ->
            block |> initTop >> bottom |> Just


expand : CursorZipper a b -> Maybe (CursorZipper a b)
expand (CursorZipper focus parctx) =
    case focus of
        TerminalBlock _ _ ->
            Nothing

        CollapsedBlock (CursorZipper newfocus subctx) ->
            let
                go ctx =
                    case ctx of
                        Top ->
                            parctx

                        Down tr before after parctx_ ->
                            Down tr before after (go parctx_)
            in
                Just <| CursorZipper newfocus (go subctx)


collapse : CursorZipper a b -> Maybe (CursorZipper a b)
collapse (CursorZipper focus ctx) =
    case ctx of
        Top ->
            Nothing

        Down tr before after parctx ->
            Just <|
                CursorZipper
                    (CollapsedBlock
                        (CursorZipper
                            focus
                            (Down tr before after Top)
                        )
                    )
                    parctx


{-| multiple meanings in Nothing
-}
leftBottom : CursorZipper a b -> Maybe (CursorZipper a b)
leftBottom =
    toBlockZipper >> upTilLeftExists >> Maybe.map bottom


rightBottom : CursorZipper a b -> Maybe (CursorZipper a b)
rightBottom =
    toBlockZipper >> upTilRightExists >> Maybe.map bottom


toBlockZipper (CursorZipper bottom ctx) =
    BlockZipper (CursorBlock bottom) ctx



-- BLOCK ZIPPERS


initTop : Block a b -> BlockZipper a b
initTop =
    flip BlockZipper Top


top : BlockZipper a b -> BlockZipper a b
top =
    untilNothing up


up : BlockZipper a b -> Maybe (BlockZipper a b)
up (BlockZipper focus ctx) =
    let
        fromCollapsable collapsable =
            case collapsable of
                Block block ->
                    Left block

                LoneWord w ->
                    Right w
    in
        case ctx of
            Top ->
                Nothing

            Down tr before after parctx ->
                Just <|
                    BlockZipper
                        (ExpandedBlock tr
                            (case fromList (List.map fromCollapsable before) of
                                Right ws ->
                                    EitherZipper ws
                                        focus
                                        (List.map fromCollapsable after)

                                Left (EitherZipper before_ b_ after_) ->
                                    EitherZipper before_
                                        b_
                                        (after_
                                            ++ [ Left focus ]
                                            ++ (List.map fromCollapsable after)
                                        )
                            )
                        )
                        parctx


bottom : BlockZipper a b -> CursorZipper a b
bottom (BlockZipper focus ctx) =
    case focus of
        ExpandedBlock tr (EitherZipper before block after) ->
            bottom <|
                BlockZipper block
                    (Down tr
                        (List.map LoneWord before)
                        (List.map (fromEither Block LoneWord) after)
                        ctx
                    )

        CursorBlock cursor ->
            CursorZipper cursor ctx


left : BlockZipper a b -> Maybe (BlockZipper a b)
left (BlockZipper focus ctx) =
    case ctx of
        Top ->
            Nothing

        Down tr before after parctx ->
            let
                prevBlock revbefore_ focus_ after_ =
                    case revbefore_ of
                        [] ->
                            Nothing

                        x :: revxs ->
                            case x of
                                Block block ->
                                    Just <|
                                        BlockZipper block
                                            (Down
                                                tr
                                                (List.reverse revxs)
                                                (focus_ :: after_)
                                                parctx
                                            )

                                _ ->
                                    prevBlock
                                        revxs
                                        x
                                        (focus_ :: after_)
            in
                prevBlock (List.reverse before) (Block focus) after


right : BlockZipper a b -> Maybe (BlockZipper a b)
right (BlockZipper focus ctx) =
    case ctx of
        Top ->
            Nothing

        Down tr before after parctx ->
            let
                nextBlock before_ focus_ after_ =
                    case after_ of
                        [] ->
                            Nothing

                        x :: xs ->
                            case x of
                                Block block ->
                                    Just <|
                                        BlockZipper block
                                            (Down
                                                tr
                                                (before_ ++ [ focus_ ])
                                                xs
                                                parctx
                                            )

                                _ ->
                                    nextBlock (before_ ++ [ focus_ ]) x xs
            in
                nextBlock before (Block focus) after


upTilRightExists : BlockZipper a b -> Maybe (BlockZipper a b)
upTilRightExists =
    let
        go tz =
            case right tz of
                Nothing ->
                    up tz |> Maybe.andThen go

                x ->
                    x
    in
        go


upTilLeftExists : BlockZipper a b -> Maybe (BlockZipper a b)
upTilLeftExists =
    let
        go tz =
            case left tz of
                Nothing ->
                    up tz |> Maybe.andThen go

                x ->
                    x
    in
        go



-- VIEW


transView : Parser.TranslatedBlock -> Html msg
transView trans =
    case
        trans
            |> fullyCollapsed
            |> initCursorZipper
            |> Maybe.andThen expand
            |> Maybe.andThen rightBottom
            |> Maybe.andThen rightBottom
            |> Maybe.andThen expand
            |> Maybe.map
                (mapCursorZipper (\str -> ( str, Nothing )) identity
                    >> register
                    >> untilNothing
                        (rightBottom >> Maybe.map register)
                    >> untilNothing
                        (leftBottom >> Maybe.map register)
                )
    of
        Nothing ->
            Debug.crash "no cursor"

        Just z ->
            cursorZipperView z


topCursorZipperView =
    mapCursorZipper (\str -> ( str, Nothing )) identity
        >> register
        >> untilNothing
            (rightBottom >> Maybe.map register)
        >> untilNothing
            (leftBottom >> Maybe.map register)
        >> cursorZipperView


cursorZipperView :
    CursorZipper ( String, Maybe (CursorZipper String Word) ) Word
    -> Html msg
cursorZipperView z =
    case top (toBlockZipper z) of
        BlockZipper block _ ->
            collapsableView (Block block)


collapsableView :
    Collapsable ( String, Maybe (CursorZipper String Word) ) Word
    -> Html msg
collapsableView collapsable =
    let
        wordView w =
            div [ class "cell orig" ] [ text w ]
    in
        case collapsable of
            LoneWord w ->
                wordView w

            Block block ->
                let
                    blockView block =
                        case block of
                            ExpandedBlock ( tr, z ) bs ->
                                div [ class "cell" ]
                                    [ div [ class "row" ]
                                        [ div [] <|
                                            List.map collapsableView <|
                                                toList <|
                                                    mapEitherZipper
                                                        Block
                                                        LoneWord
                                                        bs
                                        ]
                                    , div [ class "padding" ]
                                        [ div
                                            [ class <|
                                                case z of
                                                    Nothing ->
                                                        "trans min"

                                                    Just _ ->
                                                        "trans"
                                            ]
                                            [ text tr ]
                                        ]
                                    ]

                            CursorBlock cblock ->
                                case cblock of
                                    TerminalBlock ( tr, z ) ws ->
                                        div [ class "cell" ]
                                            [ div [ class "row" ]
                                                [ div [] <|
                                                    List.map wordView <|
                                                        Nonempty.toList ws
                                                ]
                                            , div [ class "padding" ]
                                                [ div
                                                    [ class <|
                                                        case z of
                                                            Nothing ->
                                                                "trans min"

                                                            Just _ ->
                                                                "trans"
                                                    ]
                                                    [ text tr ]
                                                ]
                                            ]

                                    CollapsedBlock cz ->
                                        cursorZipperView cz
                in
                    blockView block



-- HELPERS


type Either a b
    = Left a
    | Right b


type EitherZipper a b
    = EitherZipper (List b) a (List (Either a b))


fromNonempty : Nonempty (Either a b) -> Either (EitherZipper a b) (Nonempty b)
fromNonempty (Nonempty b1 bs) =
    List.foldl
        (\ab ebs ->
            case ebs of
                Right bs ->
                    ab
                        |> mapRight
                            (Nonempty.append bs << Nonempty.fromElement)
                        |> mapLeft
                            (\a -> EitherZipper (Nonempty.toList bs) a [])

                Left (EitherZipper before a after) ->
                    Left (EitherZipper before a (after ++ [ ab ]))
        )
        (b1
            |> mapRight Nonempty.fromElement
            |> mapLeft (\a -> EitherZipper [] a [])
        )
        bs


fromList : List (Either a b) -> Either (EitherZipper a b) (List b)
fromList =
    List.foldl
        (\ab ebs ->
            case ebs of
                Right bs ->
                    ab
                        |> mapRight
                            (\b -> bs ++ [ b ])
                        |> mapLeft
                            (\a -> EitherZipper bs a [])

                Left (EitherZipper before a after) ->
                    Left (EitherZipper before a (after ++ [ ab ]))
        )
        (Right [])


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


mapEitherZipper : (a -> c) -> (b -> d) -> EitherZipper a b -> EitherZipper c d
mapEitherZipper f g (EitherZipper before focus after) =
    EitherZipper (List.map g before)
        (f focus)
        (List.map (mapLeft f >> mapRight g) after)


prependToNonempty : List a -> Nonempty a -> Nonempty a
prependToNonempty xs (Nonempty y ys) =
    case xs of
        [] ->
            Nonempty y ys

        x :: xs ->
            Nonempty x (xs ++ [ y ] ++ ys)


appendToNonempty : List a -> Nonempty a -> Nonempty a
appendToNonempty xs (Nonempty y ys) =
    Nonempty y (ys ++ xs)
