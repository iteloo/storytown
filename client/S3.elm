module S3
    exposing
        ( send
        , sendW
        , putObject
        , baseUrlFromSignedUrl
        )

import Model exposing (Web)
import Message exposing (Msg(..))
import Http
import HttpBuilder as HttpB
import RemoteData as RD
import MediaRecorder as MR
import Debug


send :
    (a -> Msg)
    -> Http.Request a
    -> Cmd Msg
send tag =
    Http.send (handleHttpError tag Nothing)


sendW :
    (Web a -> Msg)
    -> Http.Request a
    -> Cmd Msg
sendW tag =
    Http.send
        (handleHttpError
            (tag << RD.succeed)
            (Just (tag << RD.Failure << always ()))
        )


putObject reqUrl blob =
    Http.request
        { method = "PUT"
        , headers = []
        , url = reqUrl
        , body = MR.blobBody blob
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


baseUrlFromSignedUrl url =
    case List.head (String.split "?" url) of
        Nothing ->
            Debug.crash "impossible"

        Just baseUrl ->
            baseUrl


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
            showError e


showError =
    Error << toString
