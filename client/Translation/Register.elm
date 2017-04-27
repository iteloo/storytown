module Translation.Register
    exposing
        ( registerCursors
        , RegisteredCollapsable
        )

import Translation.Base exposing (..)
import Translation.Block exposing (..)
import Translation.Cursor exposing (..)
import Either exposing (Either)
import Helper


{-| [todo] move this into the main view code to avoid using Maybe
-}



-- { formatted
--     | paragraph =
--         Tuple.mapFirst
--             (Dict.map
--                 (\_ i ->
--                     { i | collapsablers i.collapsable }
--                 )
--             )
--             formatted.paragraph
-- }


type alias RegisteredCollapsable a b =
    Collapsable ( a, Maybe (CursorZipper a b) ) b


registerCursors : Collapsable a b -> RegisteredCollapsable a b
registerCursors =
    let
        register :
            CursorZipper ( a, Maybe (CursorZipper a b) ) b
            -> CursorZipper ( a, Maybe (CursorZipper a b) ) b
        register z =
            updateNodeCursorZipper
                (Tuple.mapSecond
                    (always (Just (mapCursorZipper Tuple.first identity z)))
                )
                z
    in
        initCursorZipper
            >> Either.mapRight
                (mapCursorZipper (\str -> ( str, Nothing )) identity
                    >> register
                    >> Helper.untilNothing
                        (rightBottom >> Maybe.map register)
                    >> Helper.untilNothing
                        (leftBottom >> Maybe.map register)
                    >> toBlockZipper
                    >> top
                    >> (\(BlockZipper block ctx) -> block)
                )
            >> Either.fromEither LoneLeaf Block
