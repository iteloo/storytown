module Update exposing (update)

import Model exposing (Model, init, initItemData)
import Message exposing (Msg(..))
import Routing exposing (Route(..), parsePath, makePath)
import Server exposing (toServer)
import Api exposing (Item, Login)
import List
import Dict
import Navigation as Nav
import Update.Extra.Infix exposing ((:>))
import Record


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
            { s | items = Dict.insert i.id (initItemData i) s.items } ! []

        NewToken token ->
            -- [problem] assumes token is always valid
            { s | jwt = Just token }
                ! []
                :> update (GotoRoute ItemListPage)

        ItemAdded item ->
            { s
                | addItemInput = ""
                , items = Dict.insert item.id (initItemData item) s.items
            }
                ! []

        ItemDeleted id ->
            { s | items = Dict.remove id s.items } ! []

        -- LOGIN
        LoginButton ->
            s
                ! [ toServer s.jwt NewToken <|
                        Api.postLogin (Login s.usernameInput s.passwordInput)
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
                                (ItemAdded << flip Item new)
                            <|
                                Api.postApiItem new
                          ]

        AddItemInputChange t ->
            { s | addItemInput = t } ! []

        Done id ->
            s
                ! [ toServer s.jwt (ItemDeleted << always id) <|
                        Api.deleteApiItemByItemId id
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
            { s | recordingId = Just itemid } ! [ Record.start () ]

        StopRecording ->
            -- [note] don't set to false yet since we need to wait for file
            s ! [ Record.stop () ]

        FileReady url ->
            case s.recordingId of
                Nothing ->
                    Debug.crash "This branch should not exist"

                Just itemid ->
                    let
                        update itemData =
                            { itemData | audioURL = Just url }
                    in
                        { s
                            | recordingId = Nothing
                            , items = Dict.update itemid (Maybe.map update) s.items
                        }
                            ! []

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



-- HELPER


isJust =
    Maybe.withDefault False << Maybe.map (always True)
