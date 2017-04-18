module Message exposing (..)

import Model exposing (..)
import Api
import MediaRecorder as MR
import Navigation as Nav
import Translation.Base as Trans exposing (Collapsable, Word)
import Translation.Layout as Trans exposing (Measured)
import Time
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
    | StoryMsg StoryMsg
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


type StoryMsg
    = -- UI
      TextClicked ItemId
      -- SERVER
    | StoryReceived (Web Api.Story)
      -- PLAYBACK
    | PlayButton
    | RewindButton
    | FastForwardButton
    | AudioStarted (Result () ())
    | Rewinded (Result () ())
    | PlaybackStateChanged PlaybackState
    | NextSentence Int
      -- LAYOUT
    | CollapsableChange Int (Collapsable (List (Measured String)) (Measured Word))
    | AnimationFrame Time.Time
    | LineWrapMeasured Trans.Measure Trans.Measurement


type StoryEditMsg
    = -- UI
      AddBelowButton Int
    | ApplyButton StoryId
    | CreateButton
    | DiscardButton
    | DeleteStoryButton
    | DeleteItemButton ItemId
    | ItemSourceChange Int String
      -- SERVER
    | StoryToEditReceived (Web Api.Story)
    | StoryCreatedOrUpdated
    | StoryDeleted StoryId
      -- REC: UI
    | RecordButton ItemId
      -- REC: NATIVE
    | FileReady ( String, MR.Blob )
      -- REC: SERVER
    | S3SignedRequestAudio ItemId MR.Blob String
      -- REC: S3
    | S3UploadDone String ItemId
      -- TEST
    | TestNativeStart (Result MR.Error ())
