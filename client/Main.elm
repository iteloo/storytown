module Main exposing (..)

import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task exposing (Task, perform)
import Api exposing (..)
import HttpBuilder


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
    { items : Dict Int Item
    , addItemInput : String
    , error : Maybe String
    , jwtToken : Maybe String
    }


type alias ItemId =
    Int


init : ( Model, Cmd Msg )
init =
    let
        state =
            { items = empty
            , addItemInput = ""
            , error = Nothing
            , jwtToken = Nothing
            }
        fetch =
            toProtectedServer state Initial Api.getApiItem
    in
        ( state, fetch )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = Initial (List ItemId)
    | NewItem Item
    | Delete ItemId
    | NewToken String


type FromUi
    = AddItemInputChange String
    | AddItemButton
    | Done ItemId
    | LoginButton


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        FromServer fromServer ->
            case fromServer of
                Initial itemIds ->
                    let
                        cmd : Cmd Msg
                        cmd =
                            Cmd.batch
                                <| List.map (toProtectedServer s NewItem << getApiItemByItemId) itemIds
                    in
                        ( s, cmd )

                NewItem item ->
                    { s | items = insert item.id item s.items } ! []

                Delete id ->
                    { s | items = remove id s.items } ! []

                NewToken token ->
                    { s | jwtToken = Just token } ! []

        FromUi fromUi ->
            case fromUi of
                AddItemButton ->
                    let
                        new =
                            s.addItemInput

                        cmd =
                            toProtectedServer s (\id -> NewItem (Item id new)) (postApiItem new)

                        newState =
                            { s | addItemInput = "" }
                    in
                        if new == "" then
                            update (Error "empty field") s
                        else
                            ( newState, cmd )

                AddItemInputChange t ->
                    { s | addItemInput = t } ! []

                Done id ->
                    let
                        cmd =
                            toProtectedServer s (always (Delete id)) (deleteApiItemByItemId id)
                    in
                        ( s, cmd )
                LoginButton ->
                    let
                        cmd =
                            toServer NewToken
                              (postLogin (Login "Ali Baba" "Open Sesame"))
                    in
                        ( s, cmd )

        Error msg ->
            ( { s | error = Just msg }, Cmd.none )


toServer : (a -> FromServer) -> HttpBuilder.RequestBuilder a -> Cmd Msg
toServer tag req =
  let handleResult r = case r of
        Ok r -> FromServer <| tag r
        Err e -> Error <| toString e
  in HttpBuilder.send handleResult req

toProtectedServer : Model -> (a -> FromServer) -> HttpBuilder.RequestBuilder a -> Cmd Msg
toProtectedServer s tag req =
  case s.jwtToken of
    Nothing -> Cmd.none -- [todo] present login screen
    Just token ->
      let reqWithAuth = req
            |> HttpBuilder.withHeader "Authorization" ("Bearer " ++ token)
      in toServer tag reqWithAuth


-- VIEW


view : Model -> Html Msg
view state =
    div []
        <| [ text (toString state)
           , br [] []
           ]
        ++ (List.map (viewItem << snd) (toList state.items))
        ++ [ input [ onInput (FromUi << AddItemInputChange) ] []
           , button [ onClick (FromUi AddItemButton) ] [ text "add item" ]
           , button [ onClick (FromUi LoginButton) ] [ text "login" ]
           ]


viewItem : Item -> Html Msg
viewItem item =
    div []
        <| [ text (item.text)
           , text " - "
           , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
           ]


-- HELPERS

snd (_, b) = b
