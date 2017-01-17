module Main exposing (main)

import Model exposing (Model, init)
import Message exposing (Msg(..))
import Update exposing (update)
import View exposing (..)
import Debug exposing (..)
import Navigation as Nav
import Update.Extra.Infix exposing ((:>))


main : Program Never Model Msg
main =
    Nav.program UrlChange
        { init = \loc -> init :> update (UrlChange loc)
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
