module Server exposing (send, sendW)

import Message exposing (Msg(..))
import Model exposing (..)
import Http
import HttpBuilder as HttpB
import RemoteData as RD


send :
    Maybe AuthData
    -> (a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
send auth tag =
    Http.send (handleHttpError tag Nothing)
        << reqWithAuth auth


sendW :
    Maybe AuthData
    -> (Web a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
sendW auth tag =
    Http.send
        (handleHttpError
            (tag << RD.succeed)
            (Just (tag << RD.Failure << always ()))
        )
        << reqWithAuth auth


reqWithAuth :
    Maybe AuthData
    -> HttpB.RequestBuilder a
    -> Http.Request a
reqWithAuth auth =
    HttpB.toRequest
        << case auth of
            Nothing ->
                identity

            Just auth ->
                HttpB.withHeader "Authorization" ("Bearer " ++ auth.jwt)


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
