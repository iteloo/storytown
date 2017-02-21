module Routing
    exposing
        ( Route(..)
        , parsePath
        , makePath
        , StoryEditMode(..)
        )

import UrlParser as Url exposing (..)


type alias StoryId =
    -- [todo] refactor this duplicate declaration
    Int


type Route
    = LoginPage
    | StoryPage StoryEditMode
    | Dashboard


type StoryEditMode
    = New
    | Existing StoryId


parsePath =
    Url.parseHash route


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map (StoryPage << Existing) (s "story" </> s "edit" </> int)
        , Url.map (StoryPage New) (s "story" </> s "new")
        , Url.map LoginPage (s "login")
        , Url.map Dashboard (s "dashboard")
        ]


makePath route =
    case route of
        LoginPage ->
            "/#login"

        StoryPage New ->
            "#/story/new"

        StoryPage (Existing storyid) ->
            "#/story/edit/" ++ toString storyid

        Dashboard ->
            "/#dashboard"
