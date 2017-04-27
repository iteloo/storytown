module Message exposing (..)

import Model exposing (..)
import Api
import Language exposing (Language)
import MediaRecorder as MR
import Gesture
import Translation.Base as Trans exposing (Collapsable, Word)
import Translation.Layout as Trans exposing (Measured)
import Translation.Path as Trans
import Navigation as Nav
import Time
import Bootstrap.Navbar as Navbar
import Bootstrap.Dropdown as Dropdown


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
    = LoggedInStatus (Maybe Bool)
    | UserReceived (Maybe Api.User)


type ReadyMsg
    = NavbarMsg Navbar.State
    | PageMsg PageMsg


type PageMsg
    = SignupMsg SignupMsg
    | LoginMsg LoginMsg
    | DashboardMsg DashboardMsg
    | StoryMsg StoryMsg
    | StoryEditMsg StoryEditMsg


type SignupMsg
    = EmailInputChange String
    | FirstnameInputChange String
    | LastnameInputChange String
    | SPasswordInputChange String
    | ConfirmInputChange String
    | SignupButton
    | SignupResult (Maybe String)


type LoginMsg
    = -- UI
      UsernameInputChange String
    | PasswordInputChange String
    | LoginButton
      -- SERVER
    | AuthDataReceived Api.AuthData
    | LoginFailed


type DashboardMsg
    = StoriesReceived (Web (List ( StoryId, Api.Story )))


type StoryMsg
    = -- UI
      TextClicked ItemId
    | MouseEnter Trans.FullPath
    | MouseLeave
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
    | CollapsableChange
        Int
        (Collapsable
            { trans : List (Measured String)
            , gestureSetup : Bool
            }
            (Measured Word)
        )
    | AnimationFrame Time.Time
    | LineWrapMeasured Trans.Measure Trans.Measurement
    | SetupHammerjs Time.Time
    | OnSwipe Trans.FullPath Gesture.Direction
    | GestureSetup Trans.FullPath


type StoryEditMsg
    = -- UI
      AddBelowButton Int
    | ApplyButton StoryId
    | CreateButton
    | DiscardButton
    | DeleteStoryButton
    | DeleteItemButton ItemId
    | ItemSourceChange Int String
    | SourceDdMsg Dropdown.State
    | TargetDdMsg Dropdown.State
    | SourceSelected Language
    | TargetSelected Language
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
