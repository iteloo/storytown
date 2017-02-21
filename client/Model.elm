module Model
    exposing
        ( Model
        , init
        , StoryId
        , ItemId
        , AuthData
        , UserGroup(..)
        , StoryEdit
        , Web
        , PlaybackState(..)
        , PlaybackItemState
        , isLoaded
        , isPaused
        , isPlaying
        , duration
        , currentItemState
        )

import Routing exposing (Route(..))
import Api exposing (Story, Item)
import Dict
import RemoteData as RD
import Time
import List.Zipper exposing (Zipper)


type alias Model =
    { -- LOGIN
      usernameInput : String
    , passwordInput :
        String
        -- DASHBOARD
    , stories :
        Web (List ( StoryId, Story ))
        -- ITEM LIST
    , story : Web StoryEdit
    , recordingId :
        Maybe ItemId
        -- PLAYBACK
    , playbackState :
        PlaybackState
        -- ERROR
    , error :
        Maybe String
        -- CONTEXT
    , auth : Maybe AuthData
    , route : Route
    , history : List Route
    }


init : Model
init =
    { usernameInput = ""
    , passwordInput = ""
    , stories = RD.NotAsked
    , story = RD.NotAsked
    , recordingId = Nothing
    , playbackState = NotLoaded
    , error = Nothing
    , auth =
        Nothing
        -- bogus value; will call update
    , route = LoginPage
    , history = []
    }


type alias StoryId =
    Int


type alias ItemId =
    Int


type alias AuthData =
    { jwt : String
    , group : UserGroup
    }


type UserGroup
    = Teacher
    | Student


type alias Web a =
    RD.RemoteData () a


type alias StoryEdit =
    { title : String
    , sentences : Dict.Dict Int ItemEdit
    , freshIndex : Int
    }


type alias ItemEdit =
    { text : String
    , audioUrl : Maybe String
    }


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
