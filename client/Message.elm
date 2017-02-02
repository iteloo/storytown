module Message exposing (ItemId, Msg(..))

import Api exposing (Item)
import Routing exposing (Route)
import MediaRecorder as MR
import Navigation as Nav


type alias ItemId =
    Int


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
    | Done ItemId
      -- ITEM LIST: SERVER
    | ItemIds (List ItemId)
    | ItemInfo Item
    | ItemAdded Item
    | ItemDeleted ItemId
    | ItemUpdated ItemId
      -- ITEM LIST: AUDIO: UI
    | ToggleRecording ItemId
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
