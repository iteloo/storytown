module View exposing (view)

import Model exposing (..)
import Message exposing (..)
import MyCss exposing (CssClass(..))
import Routing
import TransView
import Parser
import Helper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Regex exposing (Match)
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
import Html.CssHelpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace MyCss.storytown


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

                StoryPage s ->
                    Html.map StoryMsg (storyView s)

                StoryEditPage s ->
                    Html.map StoryEditMsg (storyEditView s)
        , footer [ class [ Footer ] ]
            [ div [ Html.Attributes.class "container" ] [ text (toString s) ] ]
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



-- {-| [note] unused now
-- -}
-- breadcrumbView : Html ReadyMsg
-- breadcrumbView =
--     nav [ class "breadcrumb" ]
--         [ a [ class "breadcrumb-item", href "#" ]
--             [ text "Home" ]
--         , a [ class "breadcrumb-item active" ]
--             [ text "Library" ]
--         ]


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
            [ Button.primary
            , Button.attrs [ href (Routing.makePath Routing.StoryNew) ]
            ]
            [ text "New Story" ]
        , case s.stories of
            RD.NotAsked ->
                br [] []

            RD.Loading ->
                text "Loading stories"

            RD.Failure _ ->
                text "Cannot load..."

            RD.Success stories ->
                ListGroup.custom <|
                    List.map storyItemView stories
        ]


storyItemView ( storyId, story ) =
    ListGroup.anchor
        [ ListGroup.attrs
            [ Html.Attributes.class "justify-content-between"
            , href (Routing.makePath (Routing.Story storyId))
            ]
        ]
        [ text story.title
        , Button.linkButton
            [ Button.success
            , Button.attrs
                [ href (Routing.makePath (Routing.StoryEdit storyId)) ]
            ]
            [ text "Edit" ]
        ]



-- STORY VIEW


storyView : StoryModel -> Html StoryMsg
storyView s =
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
                div [ class [ Table ] ] <|
                    Dict.values <|
                        Dict.map
                            (\index item -> TransView.view item.collapsable)
                            story.sentences
        ]


itemView { playbackState } index item =
    div [ class [ Row ] ]
        [ div [ class [ Cell ] ]
            [ div [ class [ Table ] ]
                [ div [ class [ Cell ] ]
                    [ TransView.view item.collapsable ]
                ]
            , div [ Html.Attributes.id "testDiv" ] <|
                List.map
                    (span [] << List.singleton << text << .match)
                <|
                    Regex.find Regex.All
                        (Regex.regex "\\w+\\s?|[^\\w\\s]\\s?")
                        loremIpsum
            ]
        ]


loremIpsum =
    "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum"



-- STORY EDIT VIEW


storyEditView : StoryEditModel -> Html StoryEditMsg
storyEditView s =
    Grid.container [] <|
        [ case s.story of
            RD.NotAsked ->
                br [] []

            RD.Loading ->
                text "Loading..."

            RD.Failure e ->
                text "Cannot load..."

            RD.Success story ->
                div [ class [ Table ] ] <|
                    Dict.values <|
                        Dict.map
                            (\index item -> itemEditView s index item)
                            story.sentences
        , button [ onClick (AddBelowButton 0) ] [ text "+" ]
        , let
            submitButton s text_ onClick_ =
                Button.button
                    [ Button.success
                    , Button.disabled <|
                        case s.story of
                            RD.Success story ->
                                (story.sentences
                                    |> Dict.values
                                    |> List.any
                                        (.text
                                            >> Parser.parseTranslatedText
                                            >> Helper.isErr
                                        )
                                )
                                    || Dict.isEmpty story.sentences

                            _ ->
                                True
                    , Button.attrs <|
                        if RD.isSuccess s.story then
                            [ onClick onClick_ ]
                        else
                            []
                    ]
                    [ text text_ ]
          in
            case s.mode of
                New ->
                    submitButton s "Create" CreateButton

                Existing storyId ->
                    submitButton s "Apply Changes" (ApplyButton storyId)
        , Button.button
            [ Button.warning
            , Button.attrs [ onClick DiscardButton ]
            ]
            [ text <|
                case s.mode of
                    New ->
                        "Discard Draft"

                    Existing _ ->
                        "Discard Changes"
            ]
        ]
            ++ case s.mode of
                New ->
                    []

                Existing _ ->
                    [ Button.button
                        [ Button.danger
                        , Button.attrs [ onClick DeleteStoryButton ]
                        ]
                        [ text "Delete Story" ]
                    ]


itemEditView { recordingId } index item =
    div [ class [ Row ] ]
        [ div [ class [ Cell ] ]
            [ button [ onClick (DeleteItemButton index) ] [ text "x" ] ]
        , div [ class [ Cell ] ]
            [ div [ class [ Flex ] ]
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
            , div [ class [ Flex ] ]
                [ textarea
                    [ placeholder "Write something..."
                    , onInput (ItemSourceChange index)
                    ]
                    [ text item.text ]
                , div [ class [ Table ] ]
                    [ case Parser.parseTranslatedText item.text of
                        Ok r ->
                            transView r

                        Err e ->
                            text e
                    ]
                ]
            ]
        ]


transView : Parser.TranslatedBlock -> Html StoryEditMsg
transView tb =
    case tb of
        Parser.L2Word w ->
            span [ class [ Cell, Orig ] ] [ text (w ++ " ") ]

        Parser.TranslatedBlock bs tr ->
            div [ class [ Cell ] ]
                [ div [ class [ Row ] ]
                    [ div [] <| List.map transView <| Nonempty.toList bs ]
                , div [ class [ SidePadding ] ]
                    [ div
                        [ class [ Hoverarea ]
                        ]
                        [ div [ class [ Padding ] ]
                            [ div [ class [ Trans ] ]
                                [ text tr ]
                            ]
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
