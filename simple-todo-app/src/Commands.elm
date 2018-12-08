module Commands exposing (..)

import Http
import Msgs exposing (Msg)
import RemoteData
import Generated.TodoAPI exposing (..)

fetchTodos : Cmd Msg
fetchTodos =
    getTodos
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchTodos

updateTodoDone : Todo -> Cmd Msg
updateTodoDone todo =
    putTodosById todo.todoId todo
        |> Http.send (Msgs.OnTodoDoneUpdate todo.todoId)

createNewTodo : Todo -> Cmd Msg
createNewTodo todo =
    postTodos todo
        |> Http.send RemoteData.fromResult
        |> Cmd.map Msgs.OnCreateNewTodo
