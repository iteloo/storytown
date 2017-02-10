module View exposing (..)

import Model exposing (Model, ItemId, PlaybackState(..))
import Message exposing (Msg(..))
import Routing exposing (Route(..))
import Api exposing (Item)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import RemoteData as RD


view : Model -> Html Msg
view s =
    div [] <|
        [ text (toString s)
        , br [] []
        , playbackView s
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


itemsView s =
    div []
        [ case s.items of
            RD.NotAsked ->
                br [] []

            RD.Loading ->
                text "Loading..."

            RD.Failure e ->
                text "Cannot load..."

            RD.Success items ->
                div [] <|
                    List.map
                        (\( _, item ) -> itemView s.recordingId item)
                        (Dict.toList items)
        , input
            [ value s.addItemInput
            , onInput AddItemInputChange
            ]
            []
        , button [ onClick AddItemButton ] [ text "add item" ]
        ]


itemView mRecording wItem =
    case wItem of
        RD.NotAsked ->
            br [] []

        RD.Loading ->
            text "Loading..."

        RD.Failure e ->
            text "Cannot load..."

        RD.Success item ->
            div [] <|
                [ text (item.text)
                , text " - "
                , button [ onClick (DeleteButton item.idKey) ] [ text "x" ]
                , case mRecording of
                    Nothing ->
                        button [ onClick (RecordButton item.idKey) ]
                            [ text "start recording" ]

                    Just recId ->
                        if recId == item.idKey then
                            button [ onClick (RecordButton item.idKey) ]
                                [ text "stop recording" ]
                        else
                            button [ disabled True ]
                                [ text "start recording" ]
                , (case item.audioUrl of
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


playbackView s =
    div []
        [ button [ onClick PlayButton ]
            [ text
                (case s.playbackState of
                    Stopped ->
                        "Play"

                    Paused _ ->
                        "Resume"

                    Playing _ ->
                        "Pause"
                )
            ]
        , button [ onClick RewindButton ] [ text "<<" ]
        , button [ onClick FastForwardButton ] [ text ">>" ]
        ]


snd ( _, b ) =
    b
