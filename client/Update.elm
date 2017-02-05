module Update exposing (update, urlChange)

import Model exposing (Model, init)
import Message exposing (Msg(..))
import Routing exposing (Route(..), parsePath, makePath)
import Server
import S3
import Api exposing (Item, Login)
import MediaRecorder as MR
import List
import Dict
import Navigation as Nav
import Update.Extra.Infix exposing ((:>))
import Task
import Http
import RemoteData as RD


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
            { s | items = RD.map (Dict.insert idKey wItem) s.items } ! []

        ItemAdded text itemId ->
            { s
                | addItemInput = ""
                , items =
                    RD.map
                        (Dict.insert itemId <| RD.succeed <| Item itemId text Nothing)
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
                newItems =
                    RD.map
                        (Dict.update
                            itemid
                            (Maybe.map
                                (RD.map
                                    (\item -> { item | audioUrl = Just baseUrl })
                                )
                            )
                        )
                        s.items

                ( wItems, cmd ) =
                    s.items
                        |> RD.update
                            (\items ->
                                case Dict.get itemid items of
                                    Nothing ->
                                        Debug.crash ("missing item with id: " ++ toString itemid)

                                    Just wItem ->
                                        wItem
                                            |> RD.update
                                                (\item ->
                                                    let
                                                        newItem =
                                                            { item | audioUrl = Just baseUrl }
                                                    in
                                                        Dict.insert itemid (RD.succeed newItem) items
                                                            ! Debug.log "upload done!"
                                                                [ Server.send
                                                                    s.jwt
                                                                    ItemUpdated
                                                                    (Api.putApiItem newItem)
                                                                ]
                                                )
                            )
            in
                { s | items = wItems |> RD.andThen identity } ! [ cmd ]

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
