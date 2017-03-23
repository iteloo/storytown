module Update exposing (update, urlChange)

import Model
    exposing
        ( Model
        , init
        , UserGroup(..)
        , StoryEdit
        , PlaybackState(..)
        , duration
        , currentItemState
        )
import Message exposing (Msg(..))
import Routing exposing (Route(..), parsePath, makePath)
import Server
import S3
import Api exposing (Item, Login)
import Audio
import MediaRecorder as MR
import List
import Dict
import Navigation as Nav
import Update.Extra.Infix exposing ((:>))
import Task
import Http
import RemoteData as RD
import Time
import Process
import List.Zipper as Zipper


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        -- LOGIN: UI
        UsernameInputChange t ->
            { s | usernameInput = t } ! []

        PasswordInputChange t ->
            { s | passwordInput = t } ! []

        LoginButton ->
            s
                ! [ Server.send
                        AuthDataReceived
                        (Api.postLogin (Login s.usernameInput s.passwordInput))
                  ]

        -- LOGIN: SERVER
        AuthDataReceived authUnsafe ->
            -- [problem] assumes token is always valid
            let
                toSafe groupUnsafe =
                    case groupUnsafe of
                        "Teacher" ->
                            Teacher

                        "Student" ->
                            Student

                        str ->
                            Debug.crash
                                ("Cannot decode UserGroup value :" ++ str)
            in
                { s
                    | auth =
                        Just { group = toSafe authUnsafe.user.group }
                }
                    ! []
                    :> gotoRoute Dashboard

        -- DASHBOARD
        StoriesReceived stories ->
            { s | stories = stories } ! []

        -- ITEM LIST: UI
        ApplyButton storyId ->
            case s.story of
                RD.Success draft ->
                    s
                        ! [ Server.send
                                (always StoryCreatedOrUpdated)
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
                                (always StoryCreatedOrUpdated)
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
                                (Item "" Nothing)
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

        -- ITEM LIST: SERVER
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
                :> gotoRoute Dashboard

        -- ITEM LIST: AUDIO: UI
        RecordButton itemid ->
            case s.recordingId of
                Nothing ->
                    startRecording itemid s

                Just itemid ->
                    stopRecording s

        -- ITEM LIST: AUDIO: NATIVE
        FileReady ( url, blob ) ->
            case s.recordingId of
                Nothing ->
                    Debug.crash "This branch should not exist"

                Just itemid ->
                    { s | recordingId = Nothing }
                        ! [ Server.send
                                (S3SignedRequestAudio itemid blob)
                                (Api.getApiS3ByDir "audio")
                          ]
                        :> updateSentences
                            (Dict.update itemid
                                (Maybe.map
                                    (\item -> { item | audioUrl = Just url })
                                )
                            )

        -- ITEM LIST: AUDIO: SERVER
        S3SignedRequestAudio itemid blob reqUrl ->
            s
                ! [ S3.send
                        (always
                            (S3UploadDone
                                (S3.baseUrlFromSignedUrl reqUrl)
                                itemid
                            )
                        )
                        (S3.putObject reqUrl blob)
                  ]

        -- ITEM LIST: AUDIO: S3
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
                                    delay (i.end - ct) (NextSentence cnt)

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
                                    ! [ delay (duration i) (NextSentence cnt) ]
                    else
                        s ! []

                _ ->
                    s ! []

        -- ROUTING
        UrlChange loc ->
            urlChange loc s

        -- ERROR
        Error msg ->
            error msg s

        UnauthorizedError ->
            error "Unauthorized!" s
                :> gotoRoute LoginPage

        -- TEST
        -- [todo] do something nontrivial
        TestNativeStart runit ->
            s ! Debug.log "in TestNativeStart" []



-- AUDIO


startRecording itemid s =
    -- [todo] adds error handling
    { s | recordingId = Just itemid }
        ! [ Task.attempt TestNativeStart (MR.start ()) ]


stopRecording s =
    -- [note] don't set to false yet since we need to wait for file
    s
        ! [ Task.attempt
                (\r ->
                    case r of
                        Err e ->
                            Debug.crash "audio file failed to be prepared"

                        Ok r ->
                            FileReady r
                )
                (MR.stop ())
          ]



-- STORY EDIT


storyFromDraft : StoryEdit -> Api.Story
storyFromDraft draft =
    { title = draft.title
    , sentences = Dict.values draft.sentences
    }



-- ROUTING


urlChange loc s =
    case parsePath loc of
        Nothing ->
            error "Cannot parse path" s

        Just route ->
            maybeReroute route s


maybeReroute route s =
    let
        finishWithoutReroute route s =
            { s | route = route, history = s.route :: s.history }
                ! []
                :> setupRoute route
    in
        case route of
            LoginPage ->
                finishWithoutReroute LoginPage s

            _ ->
                case s.auth of
                    Nothing ->
                        gotoRoute LoginPage s

                    Just auth ->
                        finishWithoutReroute route s


gotoRoute route s =
    s ! [ Nav.newUrl (makePath route) ]


setupRoute route s =
    case route of
        StoryPage (Routing.New) ->
            { s
                | story =
                    RD.succeed
                        { title = "Untitled"
                        , freshIndex = 0
                        , sentences = Dict.empty
                        }
            }
                ! []

        StoryPage (Routing.Existing storyid) ->
            { s | story = RD.Loading }
                ! [ Server.sendW
                        StoryReceived
                        (Api.getApiStoryById storyid)
                  ]

        LoginPage ->
            { s
                | usernameInput = ""
                , passwordInput = ""
            }
                ! []

        Dashboard ->
            s
                ! [ Server.sendW
                        StoriesReceived
                        Api.getApiStory
                  ]



-- ERROR


error msg s =
    { s | error = Just msg } ! []



-- HELPERS


updateStory f s =
    { s | story = RD.map f s.story } ! []


updateSentences f s =
    s ! [] :> updateStory (\sty -> { sty | sentences = f sty.sentences })


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe =
    List.foldr (\m -> Maybe.andThen (\l -> Maybe.map (flip (::) l) m))
        (Just [])


testUrls =
    [ "https://upload.wikimedia.org/wikipedia/commons/4/4f/Hu-ad%C3%B3.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/d/d3/Hu-adni.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/f/fe/Hu-adekv%C3%A1t.ogg"
    ]
