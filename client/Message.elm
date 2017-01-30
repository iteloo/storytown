module Message exposing (ItemId, Msg(..))

import Api exposing (Item)
import Routing exposing (Route)
import MediaRecorder as MR
import Navigation as Nav


type alias ItemId =
    Int


type Msg
    = -- FROM SERVER
      ItemIds (List ItemId)
    | ItemInfo Item
    | NewToken String
    | ItemAdded Item
    | ItemDeleted ItemId
    | ItemUpdated ItemId
      -- LOGIN
    | UsernameInputChange String
    | PasswordInputChange String
    | LoginButton
      -- ITEM LIST
    | AddItemInputChange String
    | AddItemButton
    | Done ItemId
    | ToggleRecording ItemId
    | StartRecording ItemId
    | StopRecording
    | FileReady ( String, MR.Blob )
    | S3SignedRequestAudio ItemId MR.Blob String
    | S3UploadDone String ItemId
      -- CONTEXT
    | Error String
    | UrlChange Nav.Location
    | SetupRoute Route
    | GotoRoute Route
    | UnauthorizedError
      -- TEST
    | TestNativeStart (Result MR.Error ())
