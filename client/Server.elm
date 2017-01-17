module Server exposing (toServer)

import Http
import HttpBuilder as HttpB


toServer :
    Maybe String
    -> (Result Http.Error a -> msg)
    -> HttpB.RequestBuilder a
    -> Cmd msg
toServer jwt tag req =
    toServerRaw tag <|
        case jwt of
            Nothing ->
                req

            Just jwt ->
                req |> HttpB.withHeader "Authorization" ("Bearer " ++ jwt)


toServerRaw : (Result Http.Error a -> msg) -> HttpB.RequestBuilder a -> Cmd msg
toServerRaw tag req =
    HttpB.send tag req
