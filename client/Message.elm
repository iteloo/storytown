module Message exposing (..)

import Model exposing (..)
import Api
import MediaRecorder as MR
import Navigation as Nav
import RemoteData as RD
import Http
import Bootstrap.Navbar as Navbar


type Msg
    = -- ROUTING
      UrlChange Nav.Location
      -- ERROR
    | Error String
    | UnauthorizedError
      -- CHILDREN
    | NotReadyMsg NotReadyMsg
    | ReadyMsg ReadyMsg


type NotReadyMsg
    = UserReceived Api.User


type ReadyMsg
    = NavbarMsg Navbar.State
    | PageMsg PageMsg


type PageMsg
    = LoginMsg LoginMsg
    | DashboardMsg DashboardMsg
    | StoryEditMsg StoryEditMsg


type LoginMsg
    = -- UI
      UsernameInputChange String
    | PasswordInputChange String
    | LoginButton
      -- SERVER
    | AuthDataReceived Api.AuthData


type DashboardMsg
    = StoriesReceived (Web (List ( StoryId, Api.Story )))


type StoryEditMsg
    = -- UI
      AddBelowButton Int
    | ApplyButton StoryId
    | CreateButton
    | DeleteButton ItemId
    | TextClicked ItemId
    | ItemSourceChange Int String
      -- SERVER
    | StoryReceived (Web Api.Story)
    | StoryCreatedOrUpdated
      -- REC: UI
    | RecordButton ItemId
      -- REC: NATIVE
    | FileReady ( String, MR.Blob )
      -- REC: SERVER
    | S3SignedRequestAudio ItemId MR.Blob String
      -- REC: S3
    | S3UploadDone String ItemId
      -- PLAYBACK
    | PlayButton
    | RewindButton
    | FastForwardButton
    | AudioStarted (Result () ())
    | Rewinded (Result () ())
    | PlaybackStateChanged PlaybackState
    | NextSentence Int
      -- TEST
    | TestNativeStart (Result MR.Error ())
