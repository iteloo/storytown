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
    | FromItemListView IL.DefaultTag


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        FromServer fromServer ->
            case fromServer of
                ItemIds itemIds ->
                    s
                        ! List.map
                            (toServerA s ItemInfo << getApiItemByItemId)
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
                        ! [ toServerA s ItemIds Api.getApiItem ]

        FromUi fromUi ->
            case fromUi of
                FromLoginView fromLoginView ->
                    case fromLoginView of
                        Login.NewState m ->
                            ( { s | login = m }, Cmd.none )

                        Login.SubmitInfo m u p ->
                            ( { s | login = m }
                            , toServer NewToken (postLogin (Login u p))
                            )

                FromItemListView fromItemListView ->
                    case fromItemListView of
                        IL.NewState m ->
                            ( { s | itemList = m }, Cmd.none )

                        IL.AddItem m new cb ->
                            if new == "" then
                                update (Error "empty field") s
                            else
                                -- maybe clear field
                                ( { s | itemList = m }
                                , toServerA s
                                    (ForItemList
                                        << cb
                                        << flip Item new
                                    )
                                    (postApiItem new)
                                )

                        IL.DeleteItem m id cb ->
                            ( { s | itemList = m }
                            , toServerA s
                                (always <| ForItemList <| cb id)
                                (deleteApiItemByItemId id)
                            )

        Error msg ->
            ( { s | error = Just msg }, Cmd.none )



-- API HELPERS


toServer : (a -> FromServer) -> HttpB.RequestBuilder a -> Cmd Msg
toServer tag req =
    let
        handleResult r =
            case r of
                Ok r ->
                    FromServer <| tag r

                Err e ->
                    Error <| toString e
    in
        HttpB.send handleResult req


toServerA :
    Model
    -> (a -> FromServer)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
toServerA s tag req =
    case s.jwt of
        Nothing ->
            -- [todo] present login screen
            Cmd.none

        Just token ->
            let
                reqWithAuth =
                    req
                        |> HttpB.withHeader "Authorization" ("Bearer " ++ token)
            in
                toServer tag reqWithAuth



-- VIEW


view : Model -> Html Msg
view s =
    div [] <|
        [ text (toString s)
        , br [] []
        , case s.jwt of
            Nothing ->
                Login.view
                    (Login.defaultConfig (FromUi << FromLoginView))
                    s.login

            Just jwt ->
                IL.view
                    (IL.defaultConfig (FromUi << FromItemListView))
                    s.itemList
        ]
