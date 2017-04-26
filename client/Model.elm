module Model exposing (..)

import Routing
import Api
import Dict
import RemoteData as RD
import Time
import Navigation as Nav
import Bootstrap.Navbar as Navbar
import Bootstrap.Dropdown as Dropdown
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
    = LandingPage LandingModel
    | LoginPage LoginModel
    | SignupPage SignupModel
    | Dashboard DashboardModel
    | StoryPage StoryModel
    | StoryEditPage StoryEditModel
    | LoggedoutPage
    | NotFoundPage NotFoundModel


type alias LandingModel =
    { user : Maybe User }


initLanding : Maybe User -> LandingModel
initLanding user =
    { user = user }


type alias LoginModel =
    { usernameInput : String
    , passwordInput : String
    , redirect : Maybe Routing.Route
    , loginError : Bool
    , user : Maybe User
    }


initLogin : LoginModel
initLogin =
    { usernameInput = ""
    , passwordInput = ""
    , redirect = Nothing
    , loginError = False
    , user = Nothing
    }


initLoginWithUser : User -> LoginModel
initLoginWithUser user =
    { initLogin | user = Just user }


type alias SignupModel =
    { emailInput : String
    , firstnameInput : String
    , lastnameInput : String
    , passwordInput : String
    , confirmInput : String
    , user : Maybe User
    }


initSignup : Maybe User -> SignupModel
initSignup user =
    { emailInput = ""
    , firstnameInput = ""
    , lastnameInput = ""
    , passwordInput = ""
    , confirmInput = ""
    , user = user
    }


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
    , sentences : Trans.ParagraphLayout
    }


initStory : User -> StoryModel
initStory user =
    { story = RD.NotAsked
    , playbackState = NotLoaded
    , user = user
    }


type alias StoryEditModel =
    { story : Web StoryDraft
    , recordingId : Maybe ItemId
    , sourceDdState : Dropdown.State
    , targetDdState : Dropdown.State
    , user : User
    , mode : StoryEditMode
    }


type alias StoryDraft =
    { title : String
    , source : Maybe Language
    , target : Maybe Language
    , sentences : Dict.Dict Int ItemEdit
    , freshIndex : Int
    }


type Language
    = English
    | Mandarin


encodeLanguage : Language -> String
encodeLanguage =
    toString


decodeLanguage : String -> Maybe Language
decodeLanguage str =
    case str of
        "English" ->
            Just English

        "Mandarin" ->
            Just Mandarin

        _ ->
            Nothing


allLangs : List Language
allLangs =
    [ English, Mandarin ]


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
    , sourceDdState = Dropdown.initialState
    , targetDdState = Dropdown.initialState
    , user = user
    , mode = mode
    }


type alias NotFoundModel =
    { user : Maybe User }


initNotFound : Maybe User -> NotFoundModel
initNotFound user =
    { user = user }


type alias StoryId =
    Int


type alias ItemId =
    Int


type alias UserId =
    Int


type alias User =
    { firstName : String
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
        { firstName = apiUser.firstName
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
