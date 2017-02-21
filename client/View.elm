module View exposing (..)

import Model
    exposing
        ( Model
        , ItemId
        , StoryId
        , UserGroup(..)
        , PlaybackState(..)
        , isLoaded
        , isPaused
        , isPlaying
        , currentItemState
        )
import Message exposing (Msg(..))
import Routing exposing (Route(..))
import Api exposing (Item)
import Parser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import RemoteData as RD
import List.Nonempty as NList
import Json.Encode exposing (string)


view : Model -> Html Msg
view s =
    div []
        [ Html.node "link"
            [ property "rel" (string "stylesheet")
            , property "type" (string "text/css")
            , property "href" (string "style.css")
            ]
            []
        , div [] <|
            [ case s.route of
                LoginPage ->
                    loginView

                StoryPage mode ->
                    itemsView mode s

                Dashboard ->
                    dashboardView s
            , br [] []
            , br [] []
            , text (toString s)
            ]
        ]


loginView : Html Msg
loginView =
    div [] <|
        [ input [ onInput UsernameInputChange ] []
        , input [ onInput PasswordInputChange ] []
        , button [ onClick LoginButton ] [ text "login" ]
        ]



-- DASHBOARD


dashboardView s =
    case s.auth of
        Nothing ->
            br [] []

        Just auth ->
            case auth.group of
                Teacher ->
                    teacherDashboard s

                Student ->
                    text "student dashboard"


teacherDashboard s =
    div []
        [ text "Teacher Dashboard"
        , br [] []
        , a
            [ href (Routing.makePath (StoryPage Routing.New)) ]
            [ text "New Story" ]
        , case s.stories of
            RD.NotAsked ->
                br [] []

            RD.Loading ->
                text "Loading stories"

            RD.Failure _ ->
                text "Cannot load..."

            RD.Success stories ->
                ul [] <| List.map storyItemView stories
        ]


storyItemView ( storyId, story ) =
    li []
        [ a
            [ href (Routing.makePath (StoryPage (Routing.Existing storyId))) ]
            [ text story.title ]
        ]



-- ITEM VIEW


itemsView mode s =
    div []
        [ playbackView s
        , case s.story of
            RD.NotAsked ->
                br [] []

            RD.Loading ->
                text "Loading..."

            RD.Failure e ->
                text "Cannot load..."

            RD.Success story ->
                div [ class "table" ] <|
                    Dict.values <|
                        Dict.map
                            (\index item -> itemView s index item)
                            story.sentences
        , button [ onClick (AddBelowButton 0) ] [ text "+" ]
        , case mode of
            Routing.New ->
                button [ onClick CreateButton ] [ text "Create" ]

            Routing.Existing storyId ->
                button [ onClick (ApplyButton storyId) ] [ text "Apply" ]
        ]


itemView { playbackState, recordingId } index item =
    div [ class "row" ]
        [ div [ class "cell" ]
            [ button [ onClick (DeleteButton index) ] [ text "x" ] ]
        , div [ class "cell" ]
            [ div [ class "table" ]
                [ div [ class "cell" ]
                    [ case recordingId of
                        Nothing ->
                            button [ onClick (RecordButton index) ]
                                [ text "record" ]

                        Just recId ->
                            if recId == index then
                                button [ onClick (RecordButton index) ]
                                    [ text "stop rec" ]
                            else
                                button [ disabled True ]
                                    [ text "record" ]
                    ]
                , case item.audioUrl of
                    Nothing ->
                        text "No audio"

                    Just url ->
                        audio
                            [ controls True
                            , src url
                            ]
                            []
                ]
            , div [ class "table" ]
                [ div [ class "cell" ]
                    [ let
                        txt =
                            a
                                ([]
                                    ++ if
                                        isPaused playbackState
                                            || isPlaying playbackState
                                       then
                                        [ onClick (TextClicked index) ]
                                       else
                                        []
                                )
                                [ textarea
                                    [ placeholder "Write something..."
                                    , onInput (ItemSourceChange index)
                                    ]
                                    [ text item.text ]
                                ]
                      in
                        if
                            currentItemState playbackState
                                |> Maybe.map ((==) index << .itemId)
                                |> Maybe.withDefault False
                        then
                            mark []
                                [ txt ]
                        else
                            txt
                    ]
                , case Parser.parseTranslatedText item.text of
                    Ok r ->
                        transView r

                    Err e ->
                        text e
                ]
            ]
        ]


transView trans =
    div [ class "table" ]
        [ div [] <|
            List.map transBlockView trans
        ]


transBlockView : Parser.TranslatedBlock -> Html Msg
transBlockView block =
    case block of
        Parser.L2Word w ->
            div [ class "cell orig" ] [ text w ]

        Parser.TranslatedBlock bs trans ->
            div [ class "cell" ]
                [ div [ class "row" ]
                    [ div [] <|
                        NList.toList <|
                            NList.map transBlockView bs
                    ]
                , div [ class "padding" ]
                    [ div [ class "trans" ]
                        [ text trans ]
                    ]
                ]



-- PLAYBACK


playbackView { playbackState } =
    let
        disableWhenNotPlayingOrPaused =
            disabled
                (not
                    (isPaused playbackState || isPlaying playbackState)
                )
    in
        div []
            [ button
                [ onClick PlayButton
                , disabled (not (isLoaded playbackState))
                ]
                [ text
                    (case playbackState of
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
