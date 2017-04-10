module Routing
    exposing
        ( Route(..)
        , parsePath
        , makePath
        )

import UrlParser as Url exposing (..)


type alias StoryId =
    -- [todo] refactor this duplicate declaration
    Int


type Route
    = Login
    | Story StoryId
    | StoryEdit StoryId
    | StoryNew
    | Dashboard


parsePath =
    Url.parseHash route


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Story (s "story" </> int)
        , Url.map StoryEdit (s "story" </> s "edit" </> int)
        , Url.map StoryNew (s "story" </> s "new")
        , Url.map Login (s "login")
        , Url.map Dashboard (s "dashboard")
        ]


makePath route =
    case route of
        Login ->
            "#/login"

        Story storyid ->
            "#/story/" ++ toString storyid

        StoryNew ->
            "#/story/new"

        StoryEdit storyid ->
            "#/story/edit/" ++ toString storyid

        Dashboard ->
            "#/dashboard"
