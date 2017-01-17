module Routing exposing (Route(..), parsePath, makePath)

import UrlParser as Url exposing (..)


type Route
    = LoginPage
    | ItemListPage


parsePath =
    Url.parseHash route


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map ItemListPage top
        , Url.map LoginPage (s "login")
        ]


makePath route =
    case route of
        LoginPage ->
            "/#login"

        ItemListPage ->
            "/"
