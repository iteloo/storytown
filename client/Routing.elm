module Routing
    exposing
        ( Route(..)
        , parsePath
        , makePath
        )

import Navigation
import UrlParser as Url exposing (..)


type alias StoryId =
    -- [todo] refactor this duplicate declaration
    Int


type Route
    = Landing
    | Login
    | Signup
    | Story StoryId
    | StoryEdit StoryId
    | StoryNew
    | Dashboard
    | Loggedout
    | NotFound


parsePath : Navigation.Location -> Maybe Route
parsePath =
    Url.parseHash route


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Landing top
        , Url.map Login (s "login")
        , Url.map Signup (s "signup")
        , Url.map Story (s "story" </> int)
        , Url.map StoryEdit (s "story" </> s "edit" </> int)
        , Url.map StoryNew (s "story" </> s "new")
        , Url.map Dashboard (s "dashboard")
        , Url.map Loggedout (s "loggedout")
        , Url.map NotFound (s "not_found")
        ]


makePath : Route -> String
makePath route =
    case route of
        Landing ->
            "#"

        Login ->
            "#/login"

        Signup ->
            "#/signup"

        Story storyid ->
            "#/story/" ++ toString storyid

        StoryNew ->
            "#/story/new"

        StoryEdit storyid ->
            "#/story/edit/" ++ toString storyid

        Dashboard ->
            "#/dashboard"

        Loggedout ->
            "#/loggedout"

        NotFound ->
            "#/not_found"
