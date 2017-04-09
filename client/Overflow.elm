port module Overflow exposing (..)


type alias Id =
    String


type alias Measurement =
    List { top : Float, width : Float }


port checkOverflow : Id -> Cmd msg


port overflow : (Id -> msg) -> Sub msg


port measureLineWrap : Id -> Cmd msg


port lineWrapMeasured : (Measurement -> msg) -> Sub msg
