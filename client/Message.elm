module Message exposing (ItemId, Msg(..))

import Api exposing (Item)
import Routing exposing (Route)
import Navigation as Nav


type alias ItemId =
    Int


type Msg
    = -- FROM SERVER
      ItemIds (List ItemId)
    | ItemInfo Item
    | NewToken String
    | ItemAdded Item
    | ItemDeleted ItemId
      -- LOGIN
    | UsernameInputChange String
    | PasswordInputChange String
    | LoginButton
      -- ITEM LIST
    | AddItemInputChange String
    | AddItemButton
    | Done ItemId
      -- CONTEXT
    | Error String
    | UrlChange Nav.Location
    | SetupRoute Route
    | GotoRoute Route
    | UnauthorizedError
