module Model exposing (Model, init)

import Message exposing (Msg(..))
import Routing exposing (Route)
import Api exposing (Item)
import Dict
import Navigation as Nav


type alias Model =
    { -- LOGIN
      usernameInput : String
    , passwordInput :
        String
        -- ITEM LIST
    , items : Dict.Dict Int Item
    , addItemInput :
        String
        -- CONTEXT
    , error : Maybe String
    , jwt : Maybe String
    , history : List (Maybe Route)
    }


init : ( Model, Cmd Msg )
init =
    { usernameInput = ""
    , passwordInput = ""
    , items = Dict.empty
    , addItemInput = ""
    , error = Nothing
    , jwt = Nothing
    , history = []
    }
        ! []