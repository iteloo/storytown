port module Record exposing (..)


port start : () -> Cmd msg


port stop : () -> Cmd msg


port fileURL : (String -> msg) -> Sub msg
