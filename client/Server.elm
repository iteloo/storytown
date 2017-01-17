module Server exposing (toServer)

import Message exposing (Msg(..))
import Http
import HttpBuilder as HttpB


toServer :
    Maybe String
    -> (a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
toServer jwt tag req =
    toServerRaw tag <|
        case jwt of
            Nothing ->
                req

            Just jwt ->
                req |> HttpB.withHeader "Authorization" ("Bearer " ++ jwt)


toServerRaw : (a -> Msg) -> HttpB.RequestBuilder a -> Cmd Msg
toServerRaw tag req =
    let
        handleResult r =
            let
                showError e =
                    Error (toString e)
            in
                case r of
                    Ok a ->
                        tag a

                    Err e ->
                        case e of
                            Http.BadStatus resp ->
                                if resp.status.message == "Unauthorized" then
                                    UnauthorizedError
                                else
                                    showError e

                            _ ->
                                showError e
    in
        HttpB.send handleResult req
