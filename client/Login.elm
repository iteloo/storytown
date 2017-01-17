module Login
    exposing
        ( Model
        , init
        , Msg
        , update
        , view
        )

import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Html
import Server exposing (toServer)
import Http
import Context exposing (Context)


-- MODEL


type alias Model =
    ModelInternal


type alias ModelInternal =
    { usernameInput : String
    , passwordInput : String
    }


init =
    { usernameInput = ""
    , passwordInput = ""
    }



-- UPDATE


type Msg
    = UsernameInputChange String
    | PasswordInputChange String
    | LoginButton


update : Context -> Msg -> Model -> ( Model, Msg )
update ctx msg s =
    case msg of
        LoginButton ->
            s
                ! [ toServer ctx.jwt NewToken <|
                        Api.postLogin (Login s.usernameInput s.passwordInput)
                  ]

        UsernameInputChange t ->
            { s | usernameInput = t } ! []

        PasswordInputChange t ->
            { s | passwordInput = t } ! []



-- VIEW


view : Model -> Html Msg
view cfg s =
    div [] <|
        [ input [ onInput UsernameInputChange ] []
        , input [ onInput PasswordInputChange ] []
        , button [ onClick LoginButton ] [ text "login" ]
        ]
