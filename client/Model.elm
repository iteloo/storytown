module Model exposing (..)

import Routing
import Api
import Dict
import RemoteData as RD
import Time
import Navigation as Nav
import Bootstrap.Navbar as Navbar
import Translation.Base as Trans
import Translation.Layout as Trans


type alias Model =
    { app :
        AppModel
        -- ERROR
    , error : Maybe String
    }


type AppModel
    = NotReady NotReadyModel
    | Ready ReadyModel


type alias NotReadyModel =
    { startingLocation : Nav.Location
    , user : Maybe User
    }


type alias ReadyModel =
    { page : PageModel
    , navState : Navbar.State
    }


type PageModel
    = LoginPage LoginModel
    | Dashboard DashboardModel
    | StoryPage StoryModel
    | StoryEditPage StoryEditModel


type alias LoginModel =
    { usernameInput : String
    , passwordInput : String
    , redirect : Maybe Routing.Route
    , user : Maybe User
    }


initLogin : LoginModel
initLogin =
    { usernameInput = ""
    , passwordInput = ""
    , redirect = Nothing
    , user = Nothing
    }


initLoginWithUser : User -> LoginModel
initLoginWithUser user =
    { initLogin | user = Just user }


type alias DashboardModel =
    { stories : Web (List ( StoryId, Api.Story ))
    , user : User
    }


type alias StoryModel =
    { story : Web StoryState
    , playbackState : PlaybackState
    , user : User
    }


type alias StoryState =
    { title : String
    , sentences :
        Dict.Dict Int
            { collapsable : CollapsableLayout
            , audioUrl : Maybe String
            }
    }


type alias CollapsableLayout =
    -- [todo] clean up namespace
    Layout (Trans.Collapsable String Trans.Word) (Trans.Collapsable String (Trans.Measured Trans.Word)) CollapsableLayoutError


type Layout a b e
    = Raw a
    | Formatted b
    | LayoutError e


type CollapsableLayoutError
    = CannotZipWidths


initStory : User -> StoryModel
initStory user =
    { story = RD.NotAsked
    , playbackState = NotLoaded
    , user = user
    }


type alias StoryEditModel =
    { story : Web StoryDraft
    , recordingId : Maybe ItemId
    , user : User
    , mode : StoryEditMode
    }


type alias StoryDraft =
    { title : String
    , sentences : Dict.Dict Int ItemEdit
    , freshIndex : Int
    }


type StoryEditMode
    = New
    | Existing StoryId


type alias ItemEdit =
    { text : String
    , audioUrl : Maybe String
    }


initStoryEdit : User -> StoryEditMode -> StoryEditModel
initStoryEdit user mode =
    { story = RD.NotAsked
    , recordingId = Nothing
    , user = user
    , mode = mode
    }


type alias StoryId =
    Int


type alias ItemId =
    Int


type alias User =
    { userId : Int
    , firstName : String
    , lastName : String
    , email : String
    , group : UserGroup
    }


type UserGroup
    = Teacher
    | Student


apiUserToUser : Api.User -> User
apiUserToUser apiUser =
    let
        toSafe groupUnsafe =
            case groupUnsafe of
                "Teacher" ->
                    Teacher

                "Student" ->
                    Student

                str ->
                    Debug.crash
                        ("Cannot decode UserGroup value :" ++ str)
    in
        { userId = apiUser.userId
        , firstName = apiUser.firstName
        , lastName = apiUser.lastName
        , email = apiUser.email
        , group = toSafe apiUser.group
        }


type alias Web a =
    RD.RemoteData () a


type PlaybackState
    = NotLoaded
    | Stopped Int
    | Paused Int Time.Time (List PlaybackItemState)
    | Playing Int Time.Time (List PlaybackItemState)


type alias PlaybackItemState =
    { itemId : ItemId
    , start : Time.Time
    , end : Time.Time
    }


isLoaded : PlaybackState -> Bool
isLoaded ps =
    case ps of
        NotLoaded ->
            False

        _ ->
            True


isPaused : PlaybackState -> Bool
isPaused ps =
    case ps of
        Paused _ _ _ ->
            True

        _ ->
            False


isPlaying : PlaybackState -> Bool
isPlaying ps =
    case ps of
        Playing _ _ _ ->
            True

        _ ->
            False


duration : PlaybackItemState -> Time.Time
duration i =
    i.end - i.start


currentItemState : PlaybackState -> Maybe PlaybackItemState
currentItemState ps =
    let
        helper ct ts =
            List.head
                (List.filter
                    (\i -> i.start <= ct && i.end > ct)
                    ts
                )
    in
        case ps of
            Paused _ ct ts ->
                helper ct ts

            Playing _ ct ts ->
                helper ct ts

            _ ->
                Nothing
