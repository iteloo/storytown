module Model exposing (Model, init)

import Message exposing (Msg(..), Web, ItemId)
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
        -- ERROR
    , error :
        Maybe String
        -- CONTEXT
    , jwt : Maybe String
    , route : Route
    , history : List Route
    }


init : ( Model, Cmd Msg )
init =
    { usernameInput = ""
    , passwordInput = ""
    , items = RD.NotAsked
    , addItemInput = ""
    , recordingId = Nothing
    , error = Nothing
    , jwt =
        Nothing
        -- bogus value; will call update
    , route = LoginPage
    , history = []
    }
        ! []
