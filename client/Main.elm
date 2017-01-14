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
    { nav : Navigation
    , error : Maybe String
    , jwtToken : Maybe String
    }


type Navigation
    = LoginView Login.Model
    | ItemListView IL.Model


init : ( Model, Cmd Msg )
init =
    { nav = LoginView Login.init
    , error = Nothing
    , jwtToken = Nothing
    }
        ! []



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = ItemIds (List ItemId)
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
                            (toServerA s (ForItemList << IL.ItemAdded)
                                << getApiItemByItemId
                            )
                            itemIds

                ForItemList forIL ->
                    case s.nav of
                        ItemListView v ->
                            { s | nav = ItemListView <| IL.update forIL v } ! []

                        _ ->
                            { s | error = Just "No item list view! " } ! []

                NewToken token ->
                    { s
                      -- [problem] assumes token is always valid
                        | nav = ItemListView IL.init
                        , jwtToken = Just token
                    }
                        ! [ toServerA s ItemIds Api.getApiItem ]

        FromUi fromUi ->
            case fromUi of
                FromLoginView fromLoginView ->
                    case fromLoginView of
                        Login.NewState m ->
                            ( { s | nav = LoginView m }, Cmd.none )

                        Login.SubmitInfo m u p ->
                            ( { s | nav = LoginView m }
                            , toServer NewToken (postLogin (Login u p))
                            )

                FromItemListView fromItemListView ->
                    case fromItemListView of
                        IL.NewState m ->
                            ( { s | nav = ItemListView m }, Cmd.none )

                        IL.AddItem m new ->
                            if new == "" then
                                update (Error "empty field") s
                            else
                                -- maybe clear field
                                ( { s | nav = ItemListView m }
                                , toServerA s
                                    (ForItemList
                                        << IL.ItemAdded
                                        << flip Item new
                                    )
                                    (postApiItem new)
                                )

                        IL.DeleteItem m id ->
                            ( { s | nav = ItemListView m }
                            , toServerA s
                                (always <| ForItemList <| IL.ItemDeleted id)
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
    case s.jwtToken of
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
        , case s.nav of
            LoginView model ->
                Login.view
                    (Login.defaultConfig (FromUi << FromLoginView))
                    model

            ItemListView model ->
                IL.view
                    (IL.defaultConfig (FromUi << FromItemListView))
                    model
        ]
