module ItemList
    exposing
        ( Model
        , init
        , ItemId
        , Msg(..)
        , update
        , view
        , Config
        , defaultConfig
        , DefaultTag(..)
        )

import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Html
import Dict
import Api exposing (Item)


-- MODEL


type alias Model =
    { items : Dict.Dict Int Item
    , addItemInput : String
    }


init =
    { items = Dict.empty
    , addItemInput = ""
    }


type alias ItemId =
    Int



-- UPDATE


type Msg
    = ItemAdded Item
    | ItemDeleted ItemId


type MsgInternal
    = AddItemInputChange String
    | AddItemButton
    | Done ItemId


update : Msg -> Model -> Model
update msg s =
    case msg of
        ItemAdded item ->
            clearInput <| addItem item s

        ItemDeleted id ->
            deleteItemWithId id s


updateInternal : Config msg -> MsgInternal -> Model -> msg
updateInternal cfg msg s =
    case msg of
        AddItemButton ->
            cfg.addItemTag s s.addItemInput

        AddItemInputChange t ->
            cfg.newStateTag { s | addItemInput = t }

        Done id ->
            cfg.deleteItemTag s id



-- VIEW


view : Config msg -> Model -> Html msg
view cfg s =
    let
        send =
            flip (updateInternal cfg) s
    in
        div [] <|
            (List.map (viewItem (send << Done) << snd) (Dict.toList s.items))
                ++ [ input
                        [ value s.addItemInput
                        , onInput (send << AddItemInputChange)
                        ]
                        []
                   , button [ onClick (send AddItemButton) ] [ text "add item" ]
                   ]


viewItem : (ItemId -> msg) -> Item -> Html msg
viewItem tag item =
    div [] <|
        [ text (item.text)
        , text " - "
        , button [ onClick (tag item.id) ] [ text "done" ]
        ]


type alias Config msg =
    { newStateTag : Model -> msg
    , addItemTag : Model -> String -> msg
    , deleteItemTag : Model -> ItemId -> msg
    }


defaultConfig : (DefaultTag -> msg) -> Config msg
defaultConfig tag =
    { newStateTag = tag << NewState
    , addItemTag = \m -> tag << AddItem m
    , deleteItemTag = \m -> tag << DeleteItem m
    }


type DefaultTag
    = NewState Model
    | AddItem Model String
    | DeleteItem Model ItemId



-- HELPERS


setItems : List Item -> Model -> Model
setItems items s =
    { s | items = Dict.fromList <| List.map (\i -> ( i.id, i )) items }


addItem : Item -> Model -> Model
addItem i s =
    { s | items = Dict.insert i.id i s.items }


deleteItemWithId : ItemId -> Model -> Model
deleteItemWithId id s =
    { s | items = Dict.remove id s.items }


clearInput : Model -> Model
clearInput s =
    { s | addItemInput = "" }


snd ( _, b ) =
    b
