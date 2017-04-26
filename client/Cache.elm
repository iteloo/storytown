port module Cache exposing (Cache, set, get, cache, loggedIn)

import Json.Decode
import Json.Encode


type alias Cache a =
    { key : String
    , decoder : Json.Decode.Decoder a
    , encoder : a -> Json.Encode.Value
    }


loggedIn : Cache Bool
loggedIn =
    Cache "loggedIn" Json.Decode.bool Json.Encode.bool


set : Cache a -> a -> Cmd msg
set cache v =
    setRaw ( cache.key, cache.encoder v )


port setRaw : ( String, Json.Encode.Value ) -> Cmd msg


get : Cache a -> Cmd msg
get cache =
    getRaw cache.key


port getRaw : String -> Cmd msg


cache : Cache a -> (Maybe a -> msg) -> Sub msg
cache c tag =
    let
        convert =
            Maybe.map
                (\v ->
                    case Json.Decode.decodeValue c.decoder v of
                        Err e ->
                            Debug.crash
                                ("Decoding error at port boundary: " ++ e)

                        Ok a ->
                            a
                )
    in
        cacheRaw (tag << convert)


port cacheRaw : (Maybe Json.Decode.Value -> msg) -> Sub msg
