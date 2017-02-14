module Model
    exposing
        ( Model
        , init
        , ItemId
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
import Api exposing (Item)
import Dict
import RemoteData as RD
import Time
import List.Zipper exposing (Zipper)


type alias Model =
    { -- LOGIN
      usernameInput : String
    , passwordInput :
        String
        -- ITEM LIST
    , items : Web (Dict.Dict Int (Web Item))
    , addItemInput : String
    , recordingId :
        Maybe ItemId
        -- PLAYBACK
    , playbackState :
        PlaybackState
        -- ERROR
    , error :
        Maybe String
        -- CONTEXT
    , jwt : Maybe String
    , route : Route
    , history : List Route
    }


init : Model
init =
    { usernameInput = ""
    , passwordInput = ""
    , items = RD.NotAsked
    , addItemInput = ""
    , recordingId = Nothing
    , playbackState = NotLoaded
    , error = Nothing
    , jwt =
        Nothing
        -- bogus value; will call update
    , route = LoginPage
    , history = []
    }


type alias ItemId =
    Int


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
