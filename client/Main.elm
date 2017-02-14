module Main exposing (main)

import Model exposing (Model, init)
import Message exposing (Msg(..))
import Update
import View exposing (..)
import Audio
import Navigation as Nav
import Update.Extra.Infix exposing ((:>))


main : Program Never Model Msg
main =
    Nav.program UrlChange
        { init = \loc -> init ! [] :> Update.urlChange loc
        , update = Update.update
        , subscriptions = always subs
        , view = view
        }


subs =
    Audio.onStateChange PlaybackStateChanged
