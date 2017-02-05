module Message exposing (ItemId, Web, Msg(..))

import Api exposing (Item)
import Routing exposing (Route)
import MediaRecorder as MR
import Navigation as Nav
import RemoteData as RD
import Http


type alias ItemId =
    Int


type alias Web a =
    RD.RemoteData () a


type Msg
    = -- LOGIN: UI
      UsernameInputChange String
    | PasswordInputChange String
    | LoginButton
      -- LOGIN: SERVER
    | NewToken String
      -- ITEM LIST: UI
    | AddItemInputChange String
    | AddItemButton
    | DeleteButton ItemId
      -- ITEM LIST: SERVER
    | ItemIds (Web (List ItemId))
    | ItemInfo ItemId (Web Item)
    | ItemAdded String ItemId
    | ItemDeleted ItemId
    | ItemUpdated ItemId
      -- ITEM LIST: AUDIO: UI
    | RecordButton ItemId
      -- ITEM LIST: AUDIO: NATIVE
    | FileReady ( String, MR.Blob )
      -- ITEM LIST: AUDIO: SERVER
    | S3SignedRequestAudio ItemId MR.Blob String
      -- ITEM LIST: AUDIO: S3
    | S3UploadDone String ItemId
      -- ROUTING
    | UrlChange Nav.Location
    | GotoRoute Route
      -- ERROR
    | Error String
    | UnauthorizedError
      -- TEST
    | TestNativeStart (Result MR.Error ())
