module Model exposing (Model, init, ItemId, Web, PlaybackState(..), ItemState)

import Routing exposing (Route(..))
import Api exposing (Item)
import Dict
import RemoteData as RD


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
    , playbackState = Stopped
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
    = Stopped
    | Paused ItemState
    | Playing ItemState


type alias ItemState =
    { active : ItemId }
