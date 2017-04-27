port module Gesture
    exposing
        ( Id
        , Direction
        , setupHammerjs
        , hammerjsSetup
        , onSwipe
        , toDivId
        )

import Translation.Path exposing (FullPath)
import Combine exposing (string, (<*>), (<*), (*>), (<$), (<$>), many)
import Combine.Num exposing (int)
import Json.Decode
import Json.Encode


type alias Id =
    String


type Direction
    = Up
    | Down


encodeDirection : Direction -> Json.Encode.Value
encodeDirection =
    Json.Encode.string << toString


directionDecoder : Json.Decode.Decoder Direction
directionDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case s of
                    "Up" ->
                        Json.Decode.succeed Up

                    "Down" ->
                        Json.Decode.succeed Down

                    x ->
                        Json.Decode.fail
                            ("`"
                                ++ x
                                ++ "`"
                                ++ " is not a direction"
                            )
            )


setupHammerjs : FullPath -> Cmd msg
setupHammerjs =
    setupHammerjsRaw << toDivId


port setupHammerjsRaw : Id -> Cmd msg


hammerjsSetup : (FullPath -> msg) -> Sub msg
hammerjsSetup =
    let
        decode str =
            case fromDivId str of
                Nothing ->
                    Debug.crash
                        ("Error at port boundary: cannot decode div id: " ++ str)

                Just x ->
                    x
    in
        hammerjsSetupRaw << ((>>) decode)


port hammerjsSetupRaw : (Id -> msg) -> Sub msg


onSwipe : (FullPath -> Direction -> msg) -> Sub msg
onSwipe =
    let
        decodeDir val =
            case Json.Decode.decodeValue directionDecoder val of
                Err e ->
                    Debug.crash ("Error at port boundry: " ++ e)

                Ok r ->
                    r

        decodePath str =
            case fromDivId str of
                Nothing ->
                    Debug.crash
                        ("Error at port boundary: cannot decode div id: " ++ str)

                Just x ->
                    x
    in
        onSwipeRaw
            << (>>) (Tuple.mapFirst decodePath << Tuple.mapSecond decodeDir)
            << uncurry


port onSwipeRaw : (( Id, Json.Decode.Value ) -> msg) -> Sub msg


toDivId : FullPath -> String
toDivId ( idx, path ) =
    String.concat
        [ transDiv
        , toString idx
        , "-"
        , path
            |> List.map toString
            >> List.intersperse "x"
            >> List.foldr (++) ""
        ]


fromDivId : String -> Maybe FullPath
fromDivId =
    Combine.parse
        (((,)
            <$ string transDiv
            <*> int
            <* string "-"
            <*> ((::) <$> int <*> many (string "x" *> int))
         )
            <* Combine.end
        )
        >> Result.toMaybe
        >> Maybe.map (\( _, _, r ) -> r)


transDiv : String
transDiv =
    "trans"
