module Pages.TodosView exposing (..)

import Generated.TodoAPI exposing (Todo)
import Html exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as JD
import Material
import Material.List as Lists
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Models exposing (Model)
import Msgs exposing (Msg)
import RemoteData
import Utils.Events exposing (isEnter)

maybeListView : Model -> Html Msg
maybeListView model =
    case model.todos of
        RemoteData.NotAsked ->
            text "Data not asked"
        RemoteData.Loading ->
            text "Loading..."
        RemoteData.Success _ ->
            todosView model
        RemoteData.Failure error ->
            text (toString error)


todosView : Model -> Html Msg
todosView model =
    let
        notDoneTodos = List.filter (\x->x.done==False) model.todosRpl
    in
        div []
            [ Lists.ul [] (List.map (todoRow model.mdl) notDoneTodos
                            ++ [newTodoRow model]
                          )
            ]

todoRow : Material.Model -> Todo -> Html Msg
todoRow mdl todo =
    Lists.li []
        [ Lists.content [] [ text todo.title ]
        , Lists.content2 []
            [ Toggles.checkbox Msgs.Mdl [todo.todoId] mdl
                [ Options.onToggle (Msgs.ToggleDone todo.todoId)
                , Toggles.ripple
                , Toggles.value todo.done
                ] []
            ]
        ]

newTodoRow : Model -> Html Msg
newTodoRow model =
    Lists.li []
        [ Lists.content [] [newTodoField model] ]

newTodoField : Model -> Html Msg
newTodoField model =
    Textfield.render Msgs.Mdl [0] model.mdl
        [ Textfield.label "New Todo"
        , Textfield.floatingLabel
        , Textfield.autofocus
        , Options.on "keydown"
            (JD.andThen (isEnter Msgs.CreateNewTodo) keyCode)
        , Options.onInput Msgs.InputNewTodo
        , Options.id "new-todo-textfield"
        ]
        []
