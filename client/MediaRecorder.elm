module MediaRecorder exposing (..)

import Native.MediaRecorder
import Task
import Http
import Json.Decode


type Error
    = Fail


type Blob
    = Blob Json.Decode.Value


start : () -> Task.Task Error ()
start =
    Native.MediaRecorder.start


stop : () -> Task.Task Error ( String, Blob )
stop =
    Native.MediaRecorder.stop


blobPart : String -> Blob -> Http.Part
blobPart =
    Native.MediaRecorder.blobPart


blobBody : Blob -> Http.Body
blobBody =
    Native.MediaRecorder.blobBody
