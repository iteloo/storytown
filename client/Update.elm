module Update exposing (update, urlChange)

import Model
    exposing
        ( Model
        , init
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
                ! [ Server.send s.jwt
                        NewToken
                        (Api.postLogin (Login s.usernameInput s.passwordInput))
                  ]

        -- LOGIN: SERVER
        NewToken token ->
            -- [problem] assumes token is always valid
            { s | jwt = Just token }
                ! []
                :> gotoRoute ItemListPage

        -- ITEM LIST: UI
        AddItemInputChange t ->
            { s | addItemInput = t } ! []

        AddItemButton ->
            let
                new =
                    s.addItemInput
            in
                if new == "" then
                    -- [todo] add error for empty field
                    s ! []
                else
                    s
                        ! [ Server.send s.jwt
                                (ItemAdded new)
                                (Api.postApiItem new)
                          ]

        DeleteButton idKey ->
            s
                ! [ Server.send s.jwt
                        (ItemDeleted << always idKey)
                        (Api.deleteApiItemByItemId idKey)
                  ]

        TextClicked itemId ->
            s ! [ Audio.jumpTo itemId ]

        -- ITEM LIST: SERVER
        ItemIds wItemIds ->
            RD.update
                (\ids ->
                    (Dict.fromList <| List.map (\i -> ( i, RD.Loading )) ids)
                        ! List.map
                            (\idKey ->
                                Server.sendW s.jwt
                                    (ItemInfo idKey)
                                    (Api.getApiItemByItemId idKey)
                            )
                            ids
                )
                wItemIds
                |> \( wItems, cmd ) -> { s | items = wItems } ! [ cmd ]

        ItemInfo idKey wItem ->
            let
                wItems =
                    RD.map (Dict.insert idKey wItem) s.items
            in
                { s | items = wItems }
                    ! case
                        RD.toMaybe wItems
                            |> Maybe.andThen
                                (\items ->
                                    sequenceMaybe
                                        (List.map RD.toMaybe
                                            (Dict.values items)
                                        )
                                )
                      of
                        Nothing ->
                            []

                        Just items ->
                            case
                                sequenceMaybe
                                    (List.map
                                        (\i ->
                                            (i.audioUrl
                                                |> Maybe.andThen
                                                    (\url ->
                                                        Just
                                                            ( url, i.idKey )
                                                    )
                                            )
                                        )
                                        items
                                    )
                            of
                                Nothing ->
                                    []

                                Just iteminfo ->
                                    [ Audio.load iteminfo ]

        ItemAdded text itemId ->
            { s
                | addItemInput = ""
                , items =
                    RD.map
                        (Dict.insert itemId <|
                            RD.succeed <|
                                Item itemId text Nothing
                        )
                        s.items
            }
                ! []

        ItemDeleted idKey ->
            { s | items = RD.map (Dict.remove idKey) s.items } ! []

        ItemUpdated _ ->
            s ! []

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
                    { s
                        | recordingId = Nothing
                        , items =
                            RD.map
                                (Dict.update
                                    itemid
                                    (Maybe.map
                                        (RD.map
                                            (\item ->
                                                { item | audioUrl = Just url }
                                            )
                                        )
                                    )
                                )
                                s.items
                    }
                        ! [ Server.send
                                s.jwt
                                (S3SignedRequestAudio itemid blob)
                                (Api.getApiS3ByDir "audio")
                          ]

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
            let
                ( wItems, cmd ) =
                    s.items |> RD.update updateItems

                updateItems items =
                    case Dict.get itemid items of
                        Nothing ->
                            Debug.crash
                                ("missing item with id: " ++ toString itemid)

                        Just wItem ->
                            wItem |> RD.update (updateItem items)

                updateItem items item =
                    let
                        newItem =
                            { item | audioUrl = Just baseUrl }
                    in
                        Dict.insert itemid (RD.succeed newItem) items
                            ! [ Server.send s.jwt
                                    ItemUpdated
                                    (Api.putApiItem newItem)
                              ]
            in
                { s | items = wItems |> RD.andThen identity } ! [ cmd ]

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
                            List.head
                                (List.filter
                                    (\i -> i.start > old_ct)
                                    ts
                                )
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

        GotoRoute route ->
            gotoRoute route s

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
                            Debug.crash
                                "audio file failed to be prepared"

                        Ok r ->
                            FileReady r
                )
                (MR.stop ())
          ]



-- ROUTING


urlChange loc s =
    case parsePath loc of
        Nothing ->
            error "Cannot parse path" s

        Just route ->
            { s | route = route, history = s.route :: s.history }
                ! []
                :> setupRoute route


gotoRoute route s =
    s ! [ Nav.newUrl <| makePath route ]


setupRoute route s =
    case route of
        ItemListPage ->
            { s | items = RD.Loading }
                ! [ Server.sendW s.jwt ItemIds Api.getApiItem ]

        LoginPage ->
            { s
                | usernameInput = ""
                , passwordInput = ""
            }
                ! []



-- ERROR


error msg s =
    { s | error = Just msg } ! []



-- HELPERS


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe =
    List.foldr
        (\m -> Maybe.andThen (\l -> Maybe.map (flip (::) l) m))
        (Just [])


testUrls =
    [ "https://upload.wikimedia.org/wikipedia/commons/4/4f/Hu-ad%C3%B3.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/d/d3/Hu-adni.ogg"
    , "https://upload.wikimedia.org/wikipedia/commons/f/fe/Hu-adekv%C3%A1t.ogg"
    ]
