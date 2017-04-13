port module Audio exposing (..)

import Model exposing (PlaybackState(..), PlaybackItemState, ItemId)
import Json.Decode as D
import Time


port load : List ( String, ItemId ) -> Cmd msg


port play : () -> Cmd msg


port pause : () -> Cmd msg


port rewind : () -> Cmd msg


port fastforward : () -> Cmd msg


port jumpTo : ItemId -> Cmd msg


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
                << D.decodeValue playbackStateDecoder
            )


playbackStateDecoder : D.Decoder PlaybackState
playbackStateDecoder =
    (D.field "ctor" D.string)
        |> D.andThen
            (\tag ->
                case tag of
                    "Stopped" ->
                        D.map Stopped
                            (D.field "count" D.int)

                    "Paused" ->
                        D.map3 Paused
                            (D.field "count" D.int)
                            (D.field "offset" timeDecoder)
                            (D.field "timestamps" (D.list itemStateDecoder))

                    "Playing" ->
                        D.map3 Playing
                            (D.field "count" D.int)
                            (D.field "offset" timeDecoder)
                            (D.field "timestamps" (D.list itemStateDecoder))

                    _ ->
                        D.fail "not a value of PlaybackState"
            )


itemStateDecoder : D.Decoder PlaybackItemState
itemStateDecoder =
    D.map3 PlaybackItemState
        (D.field "itemId" D.int)
        (D.field "start" timeDecoder)
        (D.field "end" timeDecoder)


timeDecoder : D.Decoder Float
timeDecoder =
    D.map ((*) Time.second) D.float
