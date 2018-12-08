module View exposing (..)

import Html exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg)
import Pages.TodosView

view : Model -> Html Msg
view model =
    div []
        [ nav
        , Pages.TodosView.maybeListView model
        ]

nav : Html Msg
nav =
    div []
        [ h1 [] [ text "Simple TODO App" ] ]
