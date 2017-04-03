module View exposing (view)

import Model exposing (..)
import Message exposing (..)
import Routing
import TransView
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import RemoteData as RD
import List.Nonempty as NList
import Json.Encode as Enc
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


view : Model -> Html Msg
view s =
    div []
        [ -- [note] must come after bootstrap css
          Html.node "link"
            [ property "rel" (Enc.string "stylesheet")
            , property "type" (Enc.string "text/css")
            , property "href" (Enc.string "style.css")
            ]
            []
        , case s.app of
            NotReady s ->
                text "App loading..."

            Ready s ->
                Html.map ReadyMsg (appView s)
        , text (toString s)
        ]


appView : ReadyModel -> Html ReadyMsg
appView s =
    div []
        [ menu s.navState
        , Html.map PageMsg <|
            case s.page of
                LoginPage s ->
                    Html.map LoginMsg loginView

                Dashboard s ->
                    Html.map DashboardMsg (dashboardView s)

                StoryEditPage s ->
                    Html.map StoryEditMsg (storyEditView s)
        ]


menu : Navbar.State -> Html ReadyMsg
menu s =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Storytown" ]
        |> Navbar.items
            [ Navbar.itemLink
                [ href (Routing.makePath (Routing.Dashboard)) ]
                [ text "Dashboard" ]
            ]
        |> Navbar.view s


{-| [note] unused now
-}
breadcrumbView : Html ReadyMsg
breadcrumbView =
    nav [ class "breadcrumb" ]
        [ a [ class "breadcrumb-item", href "#" ]
            [ text "Home" ]
        , a [ class "breadcrumb-item active" ]
            [ text "Library" ]
        ]


loginView : Html LoginMsg
loginView =
    Grid.container []
        [ Form.form []
            [ h2 [] [ text "Please sign in" ]
            , Input.email
                [ Input.attrs [ placeholder "email" ]
                , Input.onInput UsernameInputChange
                ]
            , Input.password
                [ Input.attrs [ placeholder "password" ]
                , Input.onInput PasswordInputChange
                ]
            , br [] []
            , Button.button
                [ Button.primary
                , Button.success
                , Button.large
                , Button.block
                , Button.attrs [ onClick LoginButton ]
                ]
                [ text "login" ]
            ]
        ]



-- DASHBOARD


dashboardView : DashboardModel -> Html DashboardMsg
dashboardView s =
    case s.user.group of
        Teacher ->
            teacherDashboard s

        Student ->
            text "student dashboard"


teacherDashboard s =
    Grid.container []
        [ h2 [] [ text "Teacher Dashboard" ]
        , Button.linkButton
            [ Button.attrs [ href (Routing.makePath (Routing.StoryNew)) ] ]
            [ text "New Story" ]
        , case s.stories of
            RD.NotAsked ->
                br [] []

            RD.Loading ->
                text "Loading stories"

            RD.Failure _ ->
                text "Cannot load..."

            RD.Success stories ->
                ListGroup.ul <| List.map storyItemView stories
        ]


storyItemView ( storyId, story ) =
    ListGroup.li []
        [ a
            [ href (Routing.makePath (Routing.StoryEdit storyId)) ]
            [ text story.title ]
        ]



-- ITEM VIEW


storyEditView : StoryEditModel -> Html StoryEditMsg
storyEditView s =
    Grid.container []
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
        , case s.mode of
            New ->
                button [ onClick CreateButton ] [ text "Create" ]

            Existing storyId ->
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
                                (if
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
                  -- , case Parser.parseTranslatedText item.text of
                  --     Ok r ->
                  --         transView r
                  --
                  --     Err e ->
                  --         text e
                , TransView.view item.collapsable
                ]
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
