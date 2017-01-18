module View exposing (..)

import Model exposing (Model)
import Message exposing (Msg(..), ItemId)
import Routing exposing (Route(..))
import Api exposing (Item)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict


view : Model -> Html Msg
view s =
    div [] <|
        [ text (toString s)
        , br [] []
        , case s.route of
            LoginPage ->
                loginView

            ItemListPage ->
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
