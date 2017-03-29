module Server exposing (send, sendW)

import Message exposing (Msg(..))
import Model exposing (..)
import CsrfCookie
import Http
import Task
import HttpBuilder as HttpB
import RemoteData as RD


send :
    (a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
send tag =
    sendWithCsrfToken (handleHttpError tag Nothing)


sendW :
    (Web a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
sendW tag =
    sendWithCsrfToken
        (handleHttpError
            (tag << RD.succeed)
            (Just (tag << RD.Failure << always ()))
        )


sendWithCsrfToken :
    (Result Http.Error a -> msg)
    -> HttpB.RequestBuilder a
    -> Cmd msg
sendWithCsrfToken handler req =
    CsrfCookie.csrfCookie ()
        |> Task.map Just
        |> Task.onError (always (Task.succeed Nothing))
        |> Task.andThen
            (\mcsrf ->
                req
                    |> (case mcsrf of
                            Nothing ->
                                identity

                            Just csrf ->
                                HttpB.withHeader "X-XSRF-TOKEN" csrf
                       )
                    |> HttpB.toTask
            )
        |> Task.attempt handler


handleHttpError :
    (a -> Msg)
    -> Maybe (Http.Error -> Msg)
    -> Result Http.Error a
    -> Msg
handleHttpError tag tagE r =
    case r of
        Ok a ->
            tag a

        Err e ->
            case Debug.log "error" e of
                Http.BadStatus resp ->
                    if resp.status.code == 401 then
                        UnauthorizedError
                    else
                        Maybe.withDefault showError tagE e

                _ ->
                    Maybe.withDefault showError tagE e


showError =
    Error << toString
