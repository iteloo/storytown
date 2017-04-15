port module Overflow exposing (..)

import Translation.Layout exposing (Measurement)


type alias Id =
    String


port measureLineWrap : ( Int, Id ) -> Cmd msg


port lineWrapMeasured : (( Int, Measurement ) -> msg) -> Sub msg
