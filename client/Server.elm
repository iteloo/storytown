module Server exposing (send, sendW)

import Message exposing (Msg(..), Web)
import Http
import HttpBuilder as HttpB
import RemoteData as RD


send :
    Maybe String
    -> (a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
send jwt tag =
    Http.send (handleHttpError tag Nothing)
        << reqWithAuth jwt


sendW :
    Maybe String
    -> (Web a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
sendW jwt tag =
    Http.send
        (handleHttpError
            (tag << RD.succeed)
            (Just (tag << RD.Failure << always ()))
        )
        << reqWithAuth jwt


reqWithAuth :
    Maybe String
    -> HttpB.RequestBuilder a
    -> Http.Request a
reqWithAuth jwt =
    HttpB.toRequest
        << case jwt of
            Nothing ->
                identity

            Just jwt ->
                HttpB.withHeader "Authorization" ("Bearer " ++ jwt)


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
            case e of
                Http.BadStatus resp ->
                    if resp.status.message == "Unauthorized" then
                        UnauthorizedError
                    else
                        showError e

                _ ->
                    showError e


showError =
    Error << toString
