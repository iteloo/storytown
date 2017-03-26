module Update exposing (init, subs, update)

import Model exposing (..)
import Message exposing (..)
import Routing
import Server
import S3
import Api
import Audio
import MediaRecorder as MR
import Dict
import Task
import Time
import Process
import Navigation as Nav
import Update.Extra.Infix exposing ((:>))
import RemoteData as RD


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


subs =
    Audio.onStateChange (ReadyMsg << StoryEditMsg << PlaybackStateChanged)


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
                    let
                        ( newS, cmd ) =
                            updateReady msg app
                    in
                        { s | app = Ready newS } ! [ cmd ]


updateReady : ReadyMsg -> ReadyModel -> ( ReadyModel, Cmd Msg )
updateReady msg s =
    case ( msg, s ) of
        ( LoginMsg msg, LoginPage s ) ->
            let
                ( newS, cmd ) =
                    updateLogin { toMsg = ReadyMsg << LoginMsg } msg s
            in
                LoginPage newS ! [ cmd ]

        ( DashboardMsg msg, Dashboard s ) ->
            let
                ( newS, cmd ) =
                    updateDashboard { toMsg = ReadyMsg << DashboardMsg } msg s
            in
                Dashboard newS ! [ cmd ]

        ( StoryEditMsg msg, StoryEditPage s ) ->
            let
                ( newS, cmd ) =
                    updateStoryEdit { toMsg = ReadyMsg << StoryEditMsg } msg s
            in
                StoryEditPage newS ! [ cmd ]

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


updateStoryEdit :
    { toMsg : StoryEditMsg -> Msg }
    -> StoryEditMsg
    -> StoryEditModel
    -> ( StoryEditModel, Cmd Msg )
updateStoryEdit { toMsg } message s =
    case message of
        -- UI
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
                    s ! []

        CreateButton ->
            case s.story of
                RD.Success story ->
                    s
                        ! [ Server.send
                                (toMsg << always StoryCreatedOrUpdated)
                                (Api.postApiStory (storyFromDraft story))
                          ]

                _ ->
                    s ! []

        AddBelowButton index ->
            updateStory
                (\sty ->
                    { sty
                        | sentences =
                            Dict.insert sty.freshIndex
                                (Api.Item "" Nothing)
                                sty.sentences
                        , freshIndex = sty.freshIndex + 1
                    }
                )
                s

        DeleteButton index ->
            updateSentences (Dict.remove index) s

        TextClicked itemId ->
            s ! [ Audio.jumpTo itemId ]

        ItemSourceChange index txt ->
            updateSentences
                (Dict.update index
                    (Maybe.map (\item -> { item | text = txt }))
                )
                s

        -- SERVER
        StoryReceived story ->
            { s
                | story =
                    RD.map
                        (\sty ->
                            { title = sty.title
                            , sentences =
                                Dict.fromList <|
                                    List.indexedMap (,) sty.sentences
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
                    startRecording itemid s

                Just itemid ->
                    stopRecording s

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
                                    (\item -> { item | audioUrl = Just url })
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
                                ("missing item with id: " ++ toString itemid)

                        Just item ->
                            Dict.insert itemid
                                { item | audioUrl = Just baseUrl }
                                ss
                )
                s

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

        -- TEST
        -- [todo] do something nontrivial
        TestNativeStart runit ->
            s ! Debug.log "in TestNativeStart" []



-- AUDIO


startRecording itemid s =
    -- [todo] adds error handling
    { s | recordingId = Just itemid }
        ! [ Task.attempt
                (ReadyMsg << StoryEditMsg << TestNativeStart)
                (MR.start ())
          ]


stopRecording s =
    -- [note] don't set to false yet since we need to wait for file
    s
        ! [ Task.attempt
                (\r ->
                    case r of
                        Err e ->
                            Debug.crash "audio file failed to be prepared"

                        Ok r ->
                            (ReadyMsg << StoryEditMsg << FileReady) r
                )
                (MR.stop ())
          ]



-- STORY EDIT


storyFromDraft : StoryDraft -> Api.Story
storyFromDraft draft =
    { title = draft.title
    , sentences = Dict.values draft.sentences
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
                    let
                        ( newApp, cmd ) =
                            routeChange route s.app
                    in
                        { s | app = newApp } ! [ cmd ]
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
    case route of
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
                            { s
                                | story = RD.Loading
                                , mode = Existing storyid
                            }
                            ! [ Server.sendW
                                    (ReadyMsg << StoryEditMsg << StoryReceived)
                                    (Api.getApiStoryById storyid)
                              ]

        Routing.Login ->
            defaultLogin app <|
                \user ->
                    LoginPage (initLoginWithUser user) ! []

        Routing.Dashboard ->
            defaultLogin app <|
                \user ->
                    Dashboard (initDashboard user)
                        ! [ Server.sendW
                                (ReadyMsg << DashboardMsg << StoriesReceived)
                                Api.getApiStory
                          ]


defaultLogin :
    AppModel
    -> (User -> ( ReadyModel, Cmd Msg ))
    -> ( AppModel, Cmd Msg )
defaultLogin app newAppWithAuth =
    case user app of
        Nothing ->
            Ready (LoginPage initLogin) ! []

        Just user ->
            let
                ( newS, cmd ) =
                    newAppWithAuth user
            in
                Ready newS ! [ cmd ]


user : AppModel -> Maybe User
user app =
    case app of
        NotReady nr ->
            nr.user

        Ready app ->
            case app of
                LoginPage page ->
                    page.user

                Dashboard page ->
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
