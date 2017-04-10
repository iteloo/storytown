module Update exposing (init, subs, update)

import Model exposing (..)
import Message exposing (..)
import Routing
import Server
import S3
import Api
import Audio
import MediaRecorder as MR
import TransView
import Overflow
import Helper
import Dict
import Task
import Time
import Process
import Navigation as Nav
import Update.Extra exposing (addCmd)
import Update.Extra.Infix exposing ((:>))
import RemoteData as RD
import Bootstrap.Navbar as Navbar
import AnimationFrame


init : Nav.Location -> ( Model, Cmd Msg )
init loc =
    { error = Nothing
    , app =
        NotReady
            { startingLocation = loc
            , user = Nothing
            }
    }
        ! [ Server.send (NotReadyMsg << UserReceived) Api.getApiUser ]


initReady : PageModel -> ( ReadyModel, Cmd Msg )
initReady page =
    let
        ( navState, navCmd ) =
            Navbar.initialState (ReadyMsg << NavbarMsg)
    in
        { page = page
        , navState = navState
        }
            ! [ navCmd ]


initDashboard : User -> ( DashboardModel, Cmd Msg )
initDashboard user =
    { stories = RD.Loading
    , user = user
    }
        ! [ Server.sendW
                (ReadyMsg
                    << PageMsg
                    << DashboardMsg
                    << StoriesReceived
                )
                Api.getApiStory
          ]


subs s =
    Sub.batch <|
        [ Audio.onStateChange
            (ReadyMsg << PageMsg << StoryMsg << PlaybackStateChanged)
        ]
            ++ case s.app of
                Ready rm ->
                    case rm.page of
                        StoryPage _ ->
                            [ AnimationFrame.times
                                (ReadyMsg
                                    << PageMsg
                                    << StoryMsg
                                    << AnimationFrame
                                )
                            , Overflow.lineWrapMeasured
                                (ReadyMsg
                                    << PageMsg
                                    << StoryMsg
                                    << LineWrapMeasured
                                )
                            ]

                        _ ->
                            []

                _ ->
                    []


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        -- ROUTING
        UrlChange loc ->
            urlChange loc s

        -- ERROR
        Error msg ->
            error msg s

        UnauthorizedError ->
            error "Unauthorized!" s
                :> gotoRoute Routing.Login

        NotReadyMsg msg ->
            case s.app of
                NotReady nr ->
                    case msg of
                        UserReceived user ->
                            { s
                                | app =
                                    NotReady
                                        { nr
                                            | user = Just (apiUserToUser user)
                                        }
                            }
                                ! []
                                :> urlChange nr.startingLocation

                Ready _ ->
                    badMsgState message s

        ReadyMsg msg ->
            case s.app of
                NotReady _ ->
                    badMsgState message s

                Ready app ->
                    updateReady msg app
                        |> mapModel
                            (\newS -> { s | app = Ready newS })


updateReady : ReadyMsg -> ReadyModel -> ( ReadyModel, Cmd Msg )
updateReady msg s =
    case msg of
        NavbarMsg state ->
            { s | navState = state } ! []

        PageMsg msg ->
            updatePage { toMsg = ReadyMsg << PageMsg } msg s.page
                |> mapModel (\newPage -> { s | page = newPage })


updatePage :
    { toMsg : PageMsg -> Msg }
    -> PageMsg
    -> PageModel
    -> ( PageModel, Cmd Msg )
updatePage { toMsg } msg s =
    case ( msg, s ) of
        ( LoginMsg msg, LoginPage s ) ->
            mapModel LoginPage <|
                updateLogin
                    { toMsg = toMsg << LoginMsg }
                    msg
                    s

        ( DashboardMsg msg, Dashboard s ) ->
            mapModel Dashboard <|
                updateDashboard
                    { toMsg = toMsg << DashboardMsg }
                    msg
                    s

        ( StoryMsg msg, StoryPage s ) ->
            mapModel StoryPage <|
                updateStoryPage
                    { toMsg = toMsg << StoryMsg }
                    msg
                    s

        ( StoryEditMsg msg, StoryEditPage s ) ->
            mapModel StoryEditPage <|
                updateStoryEdit
                    { toMsg = toMsg << StoryEditMsg }
                    msg
                    s

        _ ->
            badMsgState msg s



-- badMsgState msg s


updateLogin :
    { toMsg : LoginMsg -> Msg }
    -> LoginMsg
    -> LoginModel
    -> ( LoginModel, Cmd Msg )
updateLogin { toMsg } msg s =
    case msg of
        -- UI
        UsernameInputChange t ->
            { s | usernameInput = t } ! []

        PasswordInputChange t ->
            { s | passwordInput = t } ! []

        LoginButton ->
            s
                ! [ Server.send
                        (toMsg << AuthDataReceived)
                        (Api.postLogin
                            (Api.Login s.usernameInput s.passwordInput)
                        )
                  ]

        -- SERVER
        AuthDataReceived authUnsafe ->
            { s | user = Just (apiUserToUser authUnsafe.user) }
                ! []
                :> gotoRoute (Maybe.withDefault Routing.Dashboard s.redirect)


updateDashboard :
    { toMsg : DashboardMsg -> Msg }
    -> DashboardMsg
    -> DashboardModel
    -> ( DashboardModel, Cmd Msg )
updateDashboard { toMsg } message s =
    case message of
        StoriesReceived stories ->
            { s | stories = stories } ! []


updateStoryPage :
    { toMsg : StoryMsg -> Msg }
    -> StoryMsg
    -> StoryModel
    -> ( StoryModel, Cmd Msg )
updateStoryPage { toMsg } message s =
    case message of
        -- UI
        TextClicked itemId ->
            s ! [ Audio.jumpTo itemId ]

        -- SERVER
        StoryReceived story ->
            let
                loadAudio s =
                    s
                        ! (RD.toMaybe s.story
                            |> Maybe.andThen
                                (.sentences
                                    >> Dict.map
                                        (\idx i ->
                                            i.audioUrl
                                                |> Maybe.map (flip (,) idx)
                                        )
                                    >> Dict.values
                                    >> Helper.sequenceMaybe
                                )
                            |> Maybe.map Audio.load
                            |> Helper.maybeToList
                          )
            in
                { s
                    | story =
                        RD.map
                            (\sty ->
                                { title = sty.title
                                , sentences =
                                    Dict.fromList <|
                                        List.indexedMap (,) <|
                                            List.map
                                                (\{ text, audioUrl } ->
                                                    { -- [tmp] bogus value
                                                      collapsable =
                                                        TransView.test
                                                    , audioUrl = audioUrl
                                                    }
                                                )
                                                sty.sentences
                                }
                            )
                            story
                }
                    ! []
                    :> loadAudio

        -- PLAYBACK
        PlayButton ->
            case s.playbackState of
                NotLoaded ->
                    s ! Debug.log "button should've been disabled" []

                Stopped _ ->
                    s ! [ Audio.play () ]

                Paused _ _ _ ->
                    s ! [ Audio.play () ]

                Playing _ _ _ ->
                    s ! [ Audio.pause () ]

        RewindButton ->
            s ! [ Audio.rewind () ]

        FastForwardButton ->
            s ! [ Audio.fastforward () ]

        AudioStarted runit ->
            -- [problem] doesn't handle error
            s ! []

        Rewinded runit ->
            s ! []

        PlaybackStateChanged ps ->
            { s | playbackState = ps }
                ! [ case ps of
                        Playing cnt ct ts ->
                            case currentItemState ps of
                                Nothing ->
                                    Debug.log "no item found" Cmd.none

                                Just i ->
                                    delay (i.end - ct)
                                        (toMsg <| NextSentence cnt)

                        _ ->
                            Cmd.none
                  ]

        NextSentence cnt ->
            case s.playbackState of
                Playing old_cnt old_ct ts ->
                    if cnt == old_cnt then
                        case
                            -- next item
                            List.head (List.filter (\i -> i.start > old_ct) ts)
                        of
                            Nothing ->
                                s ! Debug.log "no item found" []

                            Just i ->
                                { s | playbackState = Playing cnt i.start ts }
                                    ! [ delay (duration i)
                                            (toMsg <| NextSentence cnt)
                                      ]
                    else
                        s ! []

                _ ->
                    s ! []

        -- LAYOUT
        CollapsableChange new ->
            updateSentences
                (Dict.update
                    0
                    (Maybe.map
                        (\{ collapsable, audioUrl } ->
                            { collapsable = new
                            , audioUrl = audioUrl
                            }
                        )
                    )
                )
                s

        AnimationFrame _ ->
            s ! [ Overflow.measureLineWrap "testDiv" ]

        LineWrapMeasured measurement ->
            s
                ! Debug.log
                    (toString <|
                        List.foldr
                            (\sp lines ->
                                case lines of
                                    [] ->
                                        [ ( [ sp.width ], sp.top ) ]

                                    ( sps, top ) :: rest ->
                                        if sp.top == top then
                                            ( sp.width :: sps, top ) :: rest
                                        else
                                            ( [ sp.width ], sp.top ) :: lines
                            )
                            []
                            measurement
                    )
                    []


updateStoryEdit :
    { toMsg : StoryEditMsg -> Msg }
    -> StoryEditMsg
    -> StoryEditModel
    -> ( StoryEditModel, Cmd Msg )
updateStoryEdit { toMsg } message s =
    case message of
        -- UI
        AddBelowButton index ->
            updateStory
                (\sty ->
                    { sty
                        | sentences =
                            Dict.insert sty.freshIndex
                                -- [ hack ] tmp
                                (ItemEdit "" Nothing)
                                sty.sentences
                        , freshIndex = sty.freshIndex + 1
                    }
                )
                s

        ApplyButton storyId ->
            case s.story of
                RD.Success draft ->
                    s
                        ! [ Server.send
                                (toMsg << always StoryCreatedOrUpdated)
                                (Api.putApiStoryById storyId
                                    (storyFromDraft draft)
                                )
                          ]

                _ ->
                    Debug.crash "Button should be disabled!"

        CreateButton ->
            case s.story of
                RD.Success story ->
                    s
                        ! [ Server.send
                                (toMsg << always StoryCreatedOrUpdated)
                                (Api.postApiStory (storyFromDraft story))
                          ]

                _ ->
                    Debug.crash "Button should be disabled!"

        DeleteButton index ->
            updateSentences (Dict.remove index) s

        ItemSourceChange index txt ->
            updateSentences
                (Dict.update index
                    (Maybe.map (\item -> { item | text = txt }))
                )
                s

        -- SERVER
        StoryToEditReceived story ->
            { s
                | story =
                    RD.map
                        (\sty ->
                            { title = sty.title
                            , sentences =
                                Dict.fromList <|
                                    List.indexedMap (,) <|
                                        List.map
                                            (\{ text, audioUrl } ->
                                                { text = text
                                                , audioUrl = audioUrl
                                                }
                                            )
                                            sty.sentences
                            , freshIndex = List.length sty.sentences
                            }
                        )
                        story
            }
                ! []

        StoryCreatedOrUpdated ->
            { s | story = RD.NotAsked }
                ! []
                :> gotoRoute Routing.Dashboard

        -- REC: UI
        RecordButton itemid ->
            case s.recordingId of
                Nothing ->
                    -- [todo] adds error handling
                    { s | recordingId = Just itemid }
                        ! [ Task.attempt
                                (toMsg << TestNativeStart)
                                (MR.start ())
                          ]

                Just itemid ->
                    -- [note] don't set recordingId to Nothing yet
                    --        since we need to wait for file
                    s
                        ! [ Task.attempt
                                (\r ->
                                    case r of
                                        Err e ->
                                            Debug.crash <|
                                                "audio file failed "
                                                    ++ "to be prepared"

                                        Ok r ->
                                            (toMsg << FileReady) r
                                )
                                (MR.stop ())
                          ]

        -- REC: NATIVE
        FileReady ( url, blob ) ->
            case s.recordingId of
                Nothing ->
                    Debug.crash "This branch should not exist"

                Just itemid ->
                    { s | recordingId = Nothing }
                        ! [ Server.send
                                (toMsg << S3SignedRequestAudio itemid blob)
                                (Api.getApiS3ByDir "audio")
                          ]
                        :> updateSentences
                            (Dict.update itemid
                                (Maybe.map
                                    (\item ->
                                        { item | audioUrl = Just url }
                                    )
                                )
                            )

        -- REC: SERVER
        S3SignedRequestAudio itemid blob reqUrl ->
            s
                ! [ S3.send
                        (toMsg
                            << always
                                (S3UploadDone
                                    (S3.baseUrlFromSignedUrl reqUrl)
                                    itemid
                                )
                        )
                        (S3.putObject reqUrl blob)
                  ]

        -- REC: S3
        S3UploadDone baseUrl itemid ->
            updateSentences
                (\ss ->
                    case Dict.get itemid ss of
                        Nothing ->
                            Debug.crash
                                ("missing item with id: "
                                    ++ toString itemid
                                )

                        Just item ->
                            Dict.insert itemid
                                { item | audioUrl = Just baseUrl }
                                ss
                )
                s

        -- TEST
        -- [todo] do something nontrivial
        TestNativeStart runit ->
            s ! Debug.log "in TestNativeStart" []



-- STORY EDIT


storyFromDraft : StoryDraft -> Api.Story
storyFromDraft draft =
    { title = draft.title
    , sentences =
        List.map
            (\{ text, audioUrl } ->
                { text = text, audioUrl = audioUrl }
            )
        <|
            Dict.values draft.sentences
    }



-- ROUTING


gotoRoute : Routing.Route -> model -> ( model, Cmd Msg )
gotoRoute route s =
    s ! [ Nav.newUrl (Routing.makePath route) ]


urlChange : Nav.Location -> Model -> ( Model, Cmd Msg )
urlChange loc s =
    let
        go =
            case Routing.parsePath loc of
                Nothing ->
                    -- error "Cannot parse path" s
                    Debug.crash "Cannot parse path"

                Just route ->
                    routeChange route s.app
                        |> mapModel (\newApp -> { s | app = newApp })
    in
        case s.app of
            NotReady nr ->
                -- [hack] tmp; add more context in the future
                if True then
                    go
                else
                    { s | app = NotReady { nr | startingLocation = loc } } ! []

            Ready _ ->
                go


routeChange : Routing.Route -> AppModel -> ( AppModel, Cmd Msg )
routeChange route app =
    let
        ( newPage, cmd ) =
            case route of
                Routing.Story storyid ->
                    defaultLogin app <|
                        \user ->
                            let
                                s =
                                    initStory user
                            in
                                StoryPage { s | story = RD.Loading }
                                    ! [ Server.sendW
                                            (ReadyMsg
                                                << PageMsg
                                                << StoryMsg
                                                << StoryReceived
                                            )
                                            (Api.getApiStoryById storyid)
                                      , Overflow.checkOverflow "testId"
                                      ]

                Routing.StoryNew ->
                    defaultLogin app <|
                        \user ->
                            let
                                s =
                                    initStoryEdit user New
                            in
                                StoryEditPage
                                    { s
                                        | story =
                                            RD.succeed
                                                { title = "Untitled"
                                                , freshIndex = 0
                                                , sentences = Dict.empty
                                                }
                                        , mode = New
                                    }
                                    ! []

                Routing.StoryEdit storyid ->
                    defaultLogin app <|
                        \user ->
                            let
                                s =
                                    initStoryEdit user (Existing storyid)
                            in
                                StoryEditPage
                                    { s | story = RD.Loading }
                                    ! [ Server.sendW
                                            (ReadyMsg
                                                << PageMsg
                                                << StoryEditMsg
                                                << StoryToEditReceived
                                            )
                                            (Api.getApiStoryById storyid)
                                      ]

                Routing.Login ->
                    defaultLogin app <|
                        \user -> LoginPage (initLoginWithUser user) ! []

                Routing.Dashboard ->
                    defaultLogin app <|
                        \user -> mapModel Dashboard (initDashboard user)
    in
        case app of
            NotReady _ ->
                initReady newPage
                    |> mapModel Ready
                    |> addCmd cmd

            Ready app ->
                Ready { app | page = newPage } ! [ cmd ]


defaultLogin :
    AppModel
    -> (User -> ( PageModel, Cmd Msg ))
    -> ( PageModel, Cmd Msg )
defaultLogin app newPageWithAuth =
    case user app of
        Nothing ->
            LoginPage initLogin ! []

        Just user ->
            newPageWithAuth user


user : AppModel -> Maybe User
user app =
    case app of
        NotReady nr ->
            nr.user

        Ready app ->
            case app.page of
                LoginPage page ->
                    page.user

                Dashboard page ->
                    Just page.user

                StoryPage page ->
                    Just page.user

                StoryEditPage page ->
                    Just page.user



-- ERROR


error msg s =
    { s | error = Just msg } ! []



-- HELPERS


badMsgState msg s =
    Debug.log "bad msg-state combo!" ( msg, s ) |> \_ -> s ! []


updateStory f s =
    { s | story = RD.map f s.story } ! []


updateSentences f s =
    updateStory (\sty -> { sty | sentences = f sty.sentences }) s


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


mapModel : (model1 -> model2) -> ( model1, Cmd msg ) -> ( model2, Cmd msg )
mapModel f ( s, cmd ) =
    ( f s, cmd )
