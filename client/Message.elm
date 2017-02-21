module Message exposing (Msg(..))

import Model exposing (..)
import Api exposing (Item, Story)
import Routing exposing (Route)
import MediaRecorder as MR
import Navigation as Nav
import RemoteData as RD
import Http
import List.Zipper exposing (Zipper)
import Time


type Msg
    = -- LOGIN: UI
      UsernameInputChange String
    | PasswordInputChange String
    | LoginButton
      -- LOGIN: SERVER
    | AuthDataReceived Api.AuthData
      -- DASHBOARD
    | StoriesReceived (Web (List ( StoryId, Story )))
      -- ITEM LIST: UI
    | AddBelowButton Int
    | ApplyButton StoryId
    | CreateButton
    | DeleteButton ItemId
    | TextClicked ItemId
    | ItemSourceChange Int String
      -- ITEM LIST: SERVER
    | StoryReceived (Web Story)
    | StoryCreatedOrUpdated
      -- ITEM LIST: AUDIO: UI
    | RecordButton ItemId
      -- ITEM LIST: AUDIO: NATIVE
    | FileReady ( String, MR.Blob )
      -- ITEM LIST: AUDIO: SERVER
    | S3SignedRequestAudio ItemId MR.Blob String
      -- ITEM LIST: AUDIO: S3
    | S3UploadDone String ItemId
      -- PLAYBACKj
    | PlayButton
    | RewindButton
    | FastForwardButton
    | AudioStarted (Result () ())
    | Rewinded (Result () ())
    | PlaybackStateChanged PlaybackState
    | NextSentence Int
      -- ROUTING
    | UrlChange Nav.Location
      -- ERROR
    | Error String
    | UnauthorizedError
      -- TEST
    | TestNativeStart (Result MR.Error ())
