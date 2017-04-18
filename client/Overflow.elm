port module Overflow exposing (..)

import Translation.Layout exposing (Measurement, Measure, toDivId, fromDivId)


type alias Id =
    String


port measureRaw : Id -> Cmd msg


measure : Measure -> Cmd msg
measure =
    measureRaw << toDivId


port measuredRaw : (( Id, Measurement ) -> msg) -> Sub msg


measured : (( Measure, Measurement ) -> msg) -> Sub msg
measured =
    let
        unsafeFromId id =
            case fromDivId id of
                Nothing ->
                    Debug.crash ("invalid id: " ++ id)

                Just x ->
                    x
    in
        measuredRaw << ((>>) (Tuple.mapFirst unsafeFromId))
