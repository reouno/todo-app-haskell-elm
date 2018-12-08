module Msgs exposing (..)

import Generated.TodoAPI exposing (Todo)
import Http
import Material
import RemoteData exposing (WebData)

type Msg
    = OnFetchTodos (WebData (List Todo))
    | Mdl (Material.Msg Msg)
    | ToggleDone Int
    | OnTodoDoneUpdate Int (Result Http.Error ())
    | InputNewTodo String
    | CreateNewTodo
    | OnCreateNewTodo (WebData Todo)
    | FocusNewTodoTextfield

    | NoOp
