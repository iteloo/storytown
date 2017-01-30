module Update exposing (update)

import Model exposing (Model, init)
import Message exposing (Msg(..))
import Routing exposing (Route(..), parsePath, makePath)
import Server exposing (toServer)
import Api exposing (Item, Login)
import MediaRecorder as MR
import List
import Dict
import Navigation as Nav
import Update.Extra.Infix exposing ((:>))
import Task
import Http


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        -- FROM SERVER
        ItemIds itemIds ->
            s
                ! List.map
                    (toServer s.jwt ItemInfo << Api.getApiItemByItemId)
                    itemIds

        ItemInfo i ->
            { s | items = Dict.insert i.idKey i s.items } ! []

        NewToken token ->
            -- [problem] assumes token is always valid
            { s | jwt = Just token }
                ! []
                :> update (GotoRoute ItemListPage)

        ItemAdded item ->
            { s
                | addItemInput = ""
                , items = Dict.insert item.idKey item s.items
            }
                ! []

        ItemDeleted id ->
            { s | items = Dict.remove id s.items } ! []

        ItemUpdated id ->
            s ! []

        -- LOGIN
        LoginButton ->
            s
                ! [ toServer s.jwt
                        NewToken
                        (Api.postLogin (Login s.usernameInput s.passwordInput))
                  ]

        UsernameInputChange t ->
            { s | usernameInput = t } ! []

        PasswordInputChange t ->
            { s | passwordInput = t } ! []

        -- ITEM LIST
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
                        ! [ toServer s.jwt
                                (\id -> ItemAdded <| Item id new Nothing)
                                (Api.postApiItem new)
                          ]

        AddItemInputChange t ->
            { s | addItemInput = t } ! []

        Done id ->
            s
                ! [ toServer s.jwt
                        (ItemDeleted << always id)
                        (Api.deleteApiItemByItemId id)
                  ]

        ToggleRecording itemid ->
            update
                (case s.recordingId of
                    Nothing ->
                        StartRecording itemid

                    Just itemid ->
                        StopRecording
                )
                s

        StartRecording itemid ->
            -- [todo] adds error handling
            { s | recordingId = Just itemid }
                ! [ Task.attempt TestNativeStart (MR.start ()) ]

        StopRecording ->
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

        FileReady ( url, blob ) ->
            case s.recordingId of
                Nothing ->
                    Debug.crash "This branch should not exist"

                Just itemid ->
                    let
                        update item =
                            { item | audioUrl = Just url }
                    in
                        { s
                            | recordingId = Nothing
                            , items =
                                Dict.update
                                    itemid
                                    (Maybe.map update)
                                    s.items
                        }
                            ! [ toServer
                                    s.jwt
                                    (S3SignedRequestAudio itemid blob)
                                    (Api.getApiS3ByDir "audio")
                              ]

        S3SignedRequestAudio itemid blob reqUrl ->
            let
                req =
                    Http.request
                        { method = "PUT"
                        , headers = []
                        , url = reqUrl
                        , body = MR.blobBody blob
                        , expect = Http.expectStringResponse (\_ -> Ok ())
                        , timeout = Nothing
                        , withCredentials = False
                        }

                baseUrl =
                    case List.head <| String.split "?" reqUrl of
                        Nothing ->
                            Debug.crash ("no base url! :" ++ reqUrl)

                        Just url ->
                            url

                handleResult r =
                    case r of
                        Ok () ->
                            S3UploadDone baseUrl itemid

                        Err e ->
                            Debug.crash "s3 failed!"
            in
                s ! [ Http.send handleResult req ]

        S3UploadDone baseUrl itemid ->
            case Dict.get itemid s.items of
                Nothing ->
                    Debug.crash ("missing item with id: " ++ toString itemid)

                Just item ->
                    let
                        newItem =
                            { item | audioUrl = Just baseUrl }
                    in
                        { s | items = Dict.insert itemid newItem s.items }
                            ! Debug.log "upload done!"
                                [ toServer
                                    s.jwt
                                    ItemUpdated
                                    (Api.putApiItem newItem)
                                ]

        -- CONTEXT
        Error msg ->
            { s | error = Just msg } ! []

        UrlChange loc ->
            case parsePath loc of
                Nothing ->
                    update (Error "Cannot parse path") s

                Just route ->
                    { s | route = route, history = s.route :: s.history }
                        ! []
                        :> update (SetupRoute route)

        SetupRoute route ->
            case route of
                ItemListPage ->
                    s ! [ toServer s.jwt ItemIds Api.getApiItem ]

                LoginPage ->
                    { s
                        | usernameInput = ""
                        , passwordInput = ""
                    }
                        ! []

        GotoRoute route ->
            s ! [ Nav.newUrl <| makePath route ]

        -- ERROR
        UnauthorizedError ->
            update (Error "Unauthorized!") s
                :> update (GotoRoute LoginPage)

        -- TEST
        -- [todo] do something nontrivial
        TestNativeStart runit ->
            s ! Debug.log "in TestNativeStart" []
