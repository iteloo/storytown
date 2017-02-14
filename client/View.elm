module View exposing (..)

import Model
    exposing
        ( Model
        , ItemId
        , PlaybackState(..)
        , isLoaded
        , isPaused
        , isPlaying
        , currentItemState
        )
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
        [ playbackView s
        , case s.route of
            LoginPage ->
                loginView

            ItemListPage ->
                itemsView s
        , br [] []
        , br [] []
        , text (toString s)
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
                        (\( _, item ) -> itemView s item)
                        (Dict.toList items)
        , input
            [ value s.addItemInput
            , onInput AddItemInputChange
            ]
            []
        , button [ onClick AddItemButton ] [ text "add item" ]
        ]


itemView { playbackState, recordingId } wItem =
    case wItem of
        RD.NotAsked ->
            br [] []

        RD.Loading ->
            text "Loading..."

        RD.Failure e ->
            text "Cannot load..."

        RD.Success item ->
            div [] <|
                [ let
                    txt =
                        a
                            ([]
                                ++ if
                                    isPaused playbackState
                                        || isPlaying playbackState
                                   then
                                    [ onClick (TextClicked item.idKey) ]
                                   else
                                    []
                            )
                            [ text (item.text) ]
                  in
                    if
                        currentItemState playbackState
                            |> Maybe.map ((==) item.idKey << .itemId)
                            |> Maybe.withDefault False
                    then
                        mark []
                            [ txt ]
                    else
                        txt
                , text " - "
                , button [ onClick (DeleteButton item.idKey) ] [ text "x" ]
                , case recordingId of
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
    let
        disableWhenNotPlayingOrPaused =
            disabled
                (not
                    (isPaused s.playbackState || isPlaying s.playbackState)
                )
    in
        div []
            [ button
                [ onClick PlayButton
                , disabled (not (isLoaded s.playbackState))
                ]
                [ text
                    (case s.playbackState of
                        NotLoaded ->
                            "Play"

                        Stopped _ ->
                            "Play"

                        Paused _ _ _ ->
                            "Resume"

                        Playing _ _ _ ->
                            "Pause"
                    )
                ]
            , button
                [ onClick RewindButton
                , disableWhenNotPlayingOrPaused
                ]
                [ text "<<" ]
            , button
                [ onClick FastForwardButton
                , disableWhenNotPlayingOrPaused
                ]
                [ text ">>" ]
            ]


snd ( _, b ) =
    b
