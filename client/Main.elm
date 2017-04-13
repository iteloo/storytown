module Main exposing (main)

import Model
import Message
import Update
import View
import Navigation


main : Program Never Model.Model Message.Msg
main =
    Navigation.program Message.UrlChange
        { init = Update.init
        , update = Update.update
        , subscriptions = Update.subs
        , view = View.view
        }
