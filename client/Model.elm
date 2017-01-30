module Model exposing (Model, init)

import Message exposing (Msg(..), ItemId)
import Routing exposing (Route(..))
import Api exposing (Item)
import Dict


type alias Model =
    { -- LOGIN
      usernameInput : String
    , passwordInput :
        String
        -- ITEM LIST
    , items : Dict.Dict Int Item
    , addItemInput : String
    , recordingId :
        Maybe ItemId
        -- CONTEXT
    , error : Maybe String
    , jwt : Maybe String
    , route : Route
    , history : List Route
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
