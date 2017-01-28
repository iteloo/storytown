module Model exposing (Model, ItemData, initItemData, init)

import Message exposing (Msg(..), ItemId)
import Routing exposing (Route(..))
import Api exposing (Item)
import Dict
import Navigation as Nav


type alias Model =
    { -- LOGIN
      usernameInput : String
    , passwordInput :
        String
        -- ITEM LIST
    , items : Dict.Dict Int ItemData
    , addItemInput : String
    , recordingId :
        Maybe ItemId
        -- CONTEXT
    , error : Maybe String
    , jwt : Maybe String
    , route : Route
    , history : List Route
    }


type alias ItemData =
    { item : Item
    , audioURL : Maybe String
    }


initItemData item =
    { item = item
    , audioURL = Nothing
    }


init : ( Model, Cmd Msg )
init =
    { usernameInput = ""
    , passwordInput = ""
    , items = Dict.empty
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
