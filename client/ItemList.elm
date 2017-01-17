module ItemList
    exposing
        ( Model
        , init
        , ItemId
        , Msg
        , update
        , view
        , setItems
        , addItem
        , deleteItemWithId
        , clearInput
        )

import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Html
import Dict
import Api exposing (Item)
import Api
import Server exposing (toServer)
import Http
import Context exposing (Context)


-- MODEL


type alias Model =
    ModelInternal


type alias ModelInternal =
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
    = AddItemInputChange String
    | AddItemButton
    | Done ItemId
    | ItemAdded (Result Http.Error Item)
    | ItemDeleted (Result Http.Error ItemId)


update : Context -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg s =
    case msg of
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
                        ! [ toServer ctx.jwt
                                (ItemAdded << Result.map (flip Item new))
                            <|
                                Api.postApiItem new
                          ]

        AddItemInputChange t ->
            { s | addItemInput = t } ! []

        Done id ->
            s
                ! [ toServer ctx.jwt (ItemDeleted << Result.map (always id)) <|
                        Api.deleteApiItemByItemId id
                  ]

        ItemAdded (Ok item) ->
            (clearInput <| addItem item s) ! []

        ItemAdded (Err e) ->
            s ! []

        ItemDeleted (Ok id) ->
            deleteItemWithId id s ! []

        ItemDeleted (Err e) ->
            s ! []



-- VIEW


view : Model -> Html Msg
view s =
    div [] <|
        (List.map (viewItem Done << snd) (Dict.toList s.items))
            ++ [ input
                    [ value s.addItemInput
                    , onInput AddItemInputChange
                    ]
                    []
               , button [ onClick AddItemButton ] [ text "add item" ]
               ]


viewItem : (ItemId -> msg) -> Item -> Html msg
viewItem tag item =
    div [] <|
        [ text (item.text)
        , text " - "
        , button [ onClick (tag item.id) ] [ text "done" ]
        ]



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
