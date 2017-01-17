module Main exposing (..)

import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import HttpBuilder as HttpB
import Task exposing (Task, perform)
import Api exposing (..)
import List
import Login
import ItemList exposing (ItemId)
import ItemList as IL
import Server exposing (toServer)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { login : Login.Model
    , itemList : IL.Model
    , error : Maybe String
    , jwt : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    { login = Login.init
    , itemList = IL.init
    , error = Nothing
    , jwt = Nothing
    }
        ! []



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = ItemIds (List ItemId)
    | ItemInfo Item
    | ForItemList IL.Msg
    | NewToken String


type FromUi
    = FromLoginView Login.DefaultTag


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        FromServer fromServer ->
            case fromServer of
                ItemIds itemIds ->
                    s
                        ! List.map
                            (toServer s.jwt ItemInfo << getApiItemByItemId)
                            itemIds

                ItemInfo i ->
                    { s | itemList = IL.addItem i s.itemList } ! []

                ForItemList forIL ->
                    { s | itemList = IL.update forIL s.itemList } ! []

                NewToken token ->
                    { s
                      -- [problem] assumes token is always valid
                        | itemList = IL.init
                        , jwt = Just token
                    }
                        ! [ toServer s.jwt ItemIds Api.getApiItem ]

        FromUi fromUi ->
            case fromUi of
                FromLoginView fromLoginView ->
                    case fromLoginView of
                        Login.NewState m ->
                            ( { s | login = m }, Cmd.none )

                        Login.SubmitInfo m u p ->
                            ( { s | login = m }
                            , toServer s.jwt NewToken (postLogin (Login u p))
                            )

        Error msg ->
            ( { s | error = Just msg }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view s =
    div [] <|
        [ text (toString s)
        , br [] []
        , case s.jwt of
            Nothing ->
                Login.view s.login

            Just jwt ->
                IL.view s.itemList
        ]
