module Login
    exposing
        ( Model
        , init
        , DefaultTag(..)
        , view
        , Config
        , defaultConfig
        )

import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Html


-- MODEL


type alias Model =
    { usernameInput : String
    , passwordInput : String
    }


init =
    { usernameInput = ""
    , passwordInput = ""
    }



-- UPDATE


type MsgInternal
    = UsernameInputChange String
    | PasswordInputChange String
    | LoginButton


updateInternal : Config msg -> MsgInternal -> Model -> msg
updateInternal cfg msg s =
    case msg of
        LoginButton ->
            cfg.submitTag s s.usernameInput s.passwordInput

        UsernameInputChange t ->
            cfg.newStateTag { s | usernameInput = t }

        PasswordInputChange t ->
            cfg.newStateTag { s | passwordInput = t }



-- VIEW


view : Config msg -> Model -> Html msg
view cfg s =
    let
        send =
            flip (updateInternal cfg) s
    in
        div [] <|
            [ input [ onInput (send << UsernameInputChange) ] []
            , input [ onInput (send << PasswordInputChange) ] []
            , button [ onClick (send LoginButton) ] [ text "login" ]
            ]


type alias Config msg =
    { newStateTag : Model -> msg
    , submitTag : Model -> String -> String -> msg
    }


defaultConfig : (DefaultTag -> msg) -> Config msg
defaultConfig tag =
    { newStateTag = tag << NewState
    , submitTag = \m s -> tag << SubmitInfo m s
    }


type DefaultTag
    = NewState Model
    | SubmitInfo Model String String
