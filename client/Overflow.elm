port module Overflow exposing (..)


type alias Id =
    String


type alias Measurement =
    List { top : Float, width : Float }


port measureLineWrap : ( Int, Id ) -> Cmd msg


port lineWrapMeasured : (( Int, Measurement ) -> msg) -> Sub msg
