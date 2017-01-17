module Main exposing (..)

import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import HttpBuilder as HttpB
import Api exposing (Item, Login)
import List
import Dict


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { -- LOGIN
      usernameInput : String
    , passwordInput :
        String
        -- ITEM LIST
    , items : Dict.Dict Int Item
    , addItemInput :
        String
        -- CONTEXT
    , error : Maybe String
    , jwt : Maybe String
    }


type alias ItemId =
    Int


init : ( Model, Cmd Msg )
init =
    { usernameInput = ""
    , passwordInput = ""
    , items = Dict.empty
    , addItemInput = ""
    , error = Nothing
    , jwt = Nothing
    }
        ! []



-- UPDATE


type Msg
    = -- FROM SERVER
      ItemIds (List ItemId)
    | ItemInfo Item
    | NewToken String
    | ItemAdded Item
    | ItemDeleted ItemId
      -- LOGIN
    | UsernameInputChange String
    | PasswordInputChange String
    | LoginButton
      -- ITEM LIST
    | AddItemInputChange String
    | AddItemButton
    | Done ItemId
      -- CONTEXT
    | Error String


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
            { s | items = Dict.insert i.id i s.items } ! []

        NewToken token ->
            -- [problem] assumes token is always valid
            let
                t =
                    Just token
            in
                { s | jwt = t }
                    ! [ toServer t ItemIds Api.getApiItem ]

        ItemAdded item ->
            { s
                | addItemInput = ""
                , items = Dict.insert item.id item s.items
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

        -- CONTEXT
        Error msg ->
            { s | error = Just msg } ! []



-- VIEW


view : Model -> Html Msg
view s =
    div [] <|
        [ text (toString s)
        , br [] []
        , case s.jwt of
            Nothing ->
                loginView

            Just jwt ->
                itemsView s
        ]


loginView : Html Msg
loginView =
    div [] <|
        [ input [ onInput UsernameInputChange ] []
        , input [ onInput PasswordInputChange ] []
        , button [ onClick LoginButton ] [ text "login" ]
        ]


itemsView { items, addItemInput } =
    div [] <|
        (List.map (itemView Done << snd) (Dict.toList items))
            ++ [ input
                    [ value addItemInput
                    , onInput AddItemInputChange
                    ]
                    []
               , button [ onClick AddItemButton ] [ text "add item" ]
               ]


itemView : (ItemId -> msg) -> Item -> Html msg
itemView tag item =
    div [] <|
        [ text (item.text)
        , text " - "
        , button [ onClick (tag item.id) ] [ text "done" ]
        ]


snd ( _, b ) =
    b



-- SERVER HELPERS


toServer :
    Maybe String
    -> (a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
toServer jwt tag req =
    toServerRaw tag <|
        case jwt of
            Nothing ->
                req

            Just jwt ->
                req |> HttpB.withHeader "Authorization" ("Bearer " ++ jwt)


toServerRaw : (a -> Msg) -> HttpB.RequestBuilder a -> Cmd Msg
toServerRaw tag req =
    let
        handle r =
            case r of
                Ok a ->
                    tag a

                Err e ->
                    Error (toString e)
    in
        HttpB.send handle req
