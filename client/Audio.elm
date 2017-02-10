port module Audio exposing (..)

import Model exposing (PlaybackState(..), ItemState)
import Json.Decode as D


port play : () -> Cmd msg


port pause : () -> Cmd msg


port rewind : () -> Cmd msg


port fastfoward : () -> Cmd msg


port onStateChangeRaw : (D.Value -> msg) -> Sub msg


onStateChange : (PlaybackState -> msg) -> Sub msg
onStateChange tag =
    let
        handleResult r =
            case r of
                Ok r ->
                    r

                Err e ->
                    Debug.crash
                        ("cannot parse json in port boundary: onStateChange: "
                            ++ e
                        )
    in
        onStateChangeRaw
            (tag
                << handleResult
                << D.decodeValue
                    (D.string
                        |> D.andThen playbackStateDecoder
                    )
            )


playbackStateDecoder : String -> D.Decoder PlaybackState
playbackStateDecoder str =
    case str of
        "Stopped" ->
            D.succeed Stopped

        "Paused" ->
            D.map Paused itemStateDecoder

        "Playing" ->
            D.map Playing itemStateDecoder

        _ ->
            D.fail "not a value of PlaybackState"


itemStateDecoder : D.Decoder ItemState
itemStateDecoder =
    D.map ItemState
        (D.field "active" D.int)
