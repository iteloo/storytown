module View exposing (..)

import Model exposing (Model, ItemData)
import Message exposing (Msg(..), ItemId)
import Routing exposing (Route(..))
import Api exposing (Item)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict


view : Model -> Html Msg
view s =
    div [] <|
        [ text (toString s)
        , br [] []
        , case s.route of
            LoginPage ->
                loginView

            ItemListPage ->
                itemsView s
        ]


loginView : Html Msg
loginView =
    div [] <|
        [ input [ onInput UsernameInputChange ] []
        , input [ onInput PasswordInputChange ] []
        , button [ onClick LoginButton ] [ text "login" ]
        ]


itemsView { items, addItemInput, recordingId } =
    let
        toItemView ( itemid, item ) =
            itemView
                (case recordingId of
                    Nothing ->
                        False

                    Just recId ->
                        itemid == recId
                )
                item
    in
        div [] <|
            (List.map toItemView (Dict.toList items))
                ++ [ input
                        [ value addItemInput
                        , onInput AddItemInputChange
                        ]
                        []
                   , button [ onClick AddItemButton ] [ text "add item" ]
                   ]


itemView : Bool -> ItemData -> Html Msg
itemView recording item =
    div [] <|
        [ text (item.item.text)
        , text " - "
        , button [ onClick (Done item.item.id) ] [ text "done" ]
        , button [ onClick (ToggleRecording item.item.id) ]
            [ text
                (if recording then
                    "stop recording"
                 else
                    "start recording"
                )
            ]
        , (case item.audioURL of
            Nothing ->
                text "No audio"

            Just url ->
                audio
                    [ controls True
                    , src url
                    ]
                    []
          )
        ]


snd ( _, b ) =
    b
