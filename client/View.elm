module View exposing (view)

import Model exposing (..)
import Message exposing (..)
import MyCss exposing (CssClass(..))
import Routing
import TransView
import Language exposing (Language(..))
import Parser
import Helper
import Helper.StoryEdit as Helper
import Helper.Auth as Helper
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
import Bootstrap.Grid.Col as Col
import Bootstrap.Alert as Alert
import Bootstrap.Dropdown as Dropdown
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
        [ menu s.navState (Helper.user (Ready s))
        , Html.map PageMsg <|
            case s.page of
                LandingPage s ->
                    landingView

                LoginPage s ->
                    Html.map LoginMsg (loginView s)

                SignupPage s ->
                    Html.map SignupMsg (signupView s)

                Dashboard s ->
                    Html.map DashboardMsg (dashboardView s)

                StoryPage s ->
                    Html.map StoryMsg (storyView s)

                StoryEditPage s ->
                    Html.map StoryEditMsg (storyEditView s)

                LoggedoutPage ->
                    loggedoutView

                NotFoundPage _ ->
                    notFoundView
          -- , footer [ class [ Footer ] ]
          --     [ div [ Html.Attributes.class "container" ] [ text (toString s) ] ]
        ]


menu : Navbar.State -> Maybe User -> Html ReadyMsg
menu s muser =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Storytown" ]
        |> Navbar.items
            (List.concat
                [ [ Navbar.itemLink
                        [ href (Routing.makePath Routing.Dashboard) ]
                        [ text "Dashboard" ]
                  ]
                , case muser of
                    Just user ->
                        [ Navbar.dropdown
                            { id = "user-dropdown"
                            , toggle =
                                Navbar.dropdownToggle []
                                    [ text user.firstName ]
                            , items =
                                [ Navbar.dropdownHeader [ text "Profile" ]
                                , Navbar.dropdownItem []
                                    [ text
                                        (user.firstName ++ " " ++ user.lastName)
                                    ]
                                , Navbar.dropdownItem []
                                    [ text user.email ]
                                , Navbar.dropdownDivider
                                , Navbar.dropdownItem
                                    [ href (Routing.makePath Routing.Loggedout) ]
                                    [ text "Log out" ]
                                ]
                            }
                        ]

                    Nothing ->
                        [ Navbar.itemLink
                            [ href (Routing.makePath Routing.Login) ]
                            [ text "Login" ]
                        , Navbar.itemLink
                            [ href (Routing.makePath Routing.Signup) ]
                            [ text "Sign up" ]
                        ]
                ]
            )
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


landingView : Html msg
landingView =
    Grid.container []
        [ h2 []
            [ text <|
                String.concat
                    [ "Welcome to storytown. "
                    , "You can enjoy reading "
                    , "some interlinear text here. "
                    ]
            ]
        , Button.linkButton
            [ Button.success
            , Button.large
            , Button.block
            , Button.attrs [ href (Routing.makePath Routing.Login) ]
            ]
            [ text "Login" ]
        , Button.linkButton
            [ Button.success
            , Button.large
            , Button.block
            , Button.attrs [ href (Routing.makePath Routing.Signup) ]
            ]
            [ text "Sign up" ]
        ]


loginView : LoginModel -> Html LoginMsg
loginView s =
    let
        row html =
            Form.row [] [ Form.col [] [ html ] ]
    in
        Grid.container []
            [ Form.form [] <|
                List.concat
                    [ [ row <| h2 [] [ text "Please sign in with your email" ] ]
                    , if s.loginError then
                        [ row <|
                            Alert.danger
                                [ text <|
                                    String.concat
                                        [ "Cannot login. "
                                        , "Please check your email and password "
                                        , "and try again."
                                        ]
                                ]
                        ]
                      else
                        []
                    , [ row <|
                            Input.email
                                [ Input.attrs [ placeholder "you@example.com" ]
                                , Input.onInput UsernameInputChange
                                ]
                      , row <|
                            Input.password
                                [ Input.attrs [ placeholder "password" ]
                                , Input.onInput PasswordInputChange
                                ]
                      , row <|
                            Button.button
                                [ Button.primary
                                , Button.success
                                , Button.large
                                , Button.block
                                , Button.attrs [ onClick LoginButton ]
                                ]
                                [ text "login" ]
                      ]
                    ]
            ]


formValid : SignupModel -> Bool
formValid s =
    List.all (not << String.isEmpty)
        [ s.firstnameInput
        , s.lastnameInput
        , s.passwordInput
        ]
        && (s.passwordInput == s.confirmInput)


passwordValid : SignupModel -> Bool
passwordValid s =
    String.isEmpty s.confirmInput || (s.passwordInput == s.confirmInput)


signupView : SignupModel -> Html SignupMsg
signupView s =
    let
        row el label ph msg =
            Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text label ]
                , Form.col [ Col.sm10 ]
                    [ el
                        [ Input.attrs [ placeholder ph ]
                        , Input.onInput msg
                        ]
                    ]
                ]
    in
        Grid.container []
            [ Form.form []
                [ h2 [] [ text "Sign up" ]
                , row Input.email "Email" "required" EmailInputChange
                , row Input.text "First name" "required" FirstnameInputChange
                , row Input.text "Last name" "required" LastnameInputChange
                , row Input.password "Password" "required" SPasswordInputChange
                , Form.row
                    (List.concat
                        [ if passwordValid s then
                            []
                          else
                            [ Form.rowWarning ]
                        ]
                    )
                    [ Form.colLabel [ Col.sm2 ] [ text "Confirm password" ]
                    , Form.col [ Col.sm10 ]
                        [ Input.password <|
                            List.concat
                                [ [ Input.attrs [ placeholder "required" ]
                                  , Input.onInput ConfirmInputChange
                                  ]
                                , if passwordValid s then
                                    []
                                  else
                                    [ Input.warning ]
                                ]
                        ]
                    ]
                , Button.button
                    [ Button.primary
                    , Button.success
                    , Button.large
                    , Button.block
                    , Button.disabled (not (formValid s))
                    , Button.attrs [ onClick SignupButton ]
                    ]
                    [ text "Sign up" ]
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


teacherDashboard :
    { b | stories : Web (List ( Int, { a | title : String } )) }
    -> Html msg
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


storyItemView : ( StoryId, { a | title : String } ) -> ListGroup.CustomItem msg
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
                TransView.view story
                    s.playbackState
                    (currentItemState s.playbackState |> Maybe.map .itemId)
        ]


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
                div []
                    [ text "Translate from"
                    , Dropdown.dropdown
                        s.sourceDdState
                        { options = [ Dropdown.alignMenuRight ]
                        , toggleMsg = SourceDdMsg
                        , toggleButton =
                            Dropdown.toggle [ Button.secondary ]
                                [ text
                                    (story.source
                                        |> Maybe.map toString
                                        |> Maybe.withDefault "..."
                                    )
                                ]
                        , items =
                            List.map
                                (\lang ->
                                    Dropdown.buttonItem
                                        [ onClick (SourceSelected lang) ]
                                        [ text (toString lang) ]
                                )
                                Language.allLangs
                        }
                    , text "to"
                    , Dropdown.dropdown
                        s.targetDdState
                        { options = [ Dropdown.alignMenuRight ]
                        , toggleMsg = TargetDdMsg
                        , toggleButton =
                            Dropdown.toggle [ Button.secondary ]
                                [ text
                                    (story.target
                                        |> Maybe.map toString
                                        |> Maybe.withDefault "..."
                                    )
                                ]
                        , items =
                            List.map
                                (\lang ->
                                    Dropdown.buttonItem
                                        [ onClick (TargetSelected lang) ]
                                        [ text (toString lang) ]
                                )
                                Language.allLangs
                        }
                    , div [ class [ Table ] ] <|
                        Dict.values <|
                            Dict.map
                                (\index item -> itemEditView s index item)
                                story.sentences
                    ]
        , button [ onClick (AddBelowButton 0) ] [ text "+" ]
        , let
            submitButton s text_ onClick_ =
                Button.button
                    [ Button.success
                    , Button.disabled
                        (s.story
                            |> RD.toMaybe
                            |> Maybe.andThen Helper.storyFromDraft
                            |> Helper.isNothing
                        )
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


itemEditView :
    { a | recordingId : Maybe ItemId }
    -> ItemId
    -> { b | audioUrl : Maybe String, text : String }
    -> Html StoryEditMsg
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
                    [ -- [tmp] [hack] we don't need to split translations in
                      --       edit mode, so give bogus language for now
                      case Parser.parseTranslatedText Mandarin item.text of
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
            span [ class [ Cell, Orig ] ]
                (String.split " " w
                    |> List.map text
                    |> List.intersperse
                        (span [ class [ ColouredSpace ] ] [ text " " ])
                )

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


playbackView : { a | playbackState : PlaybackState } -> Html StoryMsg
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
                            "No Audio"

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



-- LOGGED OUT


loggedoutView : Html msg
loggedoutView =
    Grid.container [] [ text "You are successfully logged out!" ]



-- NOT FOUND


notFoundView : Html msg
notFoundView =
    Grid.container [] [ text "Page not found :(" ]
