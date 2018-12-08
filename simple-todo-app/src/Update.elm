module Update exposing (..)

import Commands exposing (..)
import Dom
import Material
import Models exposing (Model)
import Msgs exposing (Msg)
import RemoteData
import Task

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msgs.OnFetchTodos remoteData ->
            case remoteData of
                RemoteData.Success response ->
                    ( { model
                        | todos = RemoteData.Success []
                        , todosRpl = response
                      }
                    , Cmd.none
                    )
                _ ->
                    ( { model | todos = remoteData }, Cmd.none )

{- Material Design Lite -}
        Msgs.Mdl msg_ ->
            Material.update Msgs.Mdl msg_ model
        Msgs.ToggleDone todoId ->
            let
                mayTodo = model.todosRpl
                        |> List.filter (\x -> x.todoId == todoId)
                        |> List.head
            in
                case mayTodo of
                    Just todo ->
                        ( model
                        , updateTodoDone { todo | done = not todo.done }
                        )
                    Nothing ->
                        let _ = Debug.log "Msgs.ToggleDone: todoId not found: " "" in
                        ( model, Cmd.none )
        Msgs.OnTodoDoneUpdate todoId result ->
            case result of
                Ok response ->
                    let
                        _ = Debug.log "Msgs.OnTodoDoneUpdate Success:" response
                        toggleDone todoId todo =
                            if todo.todoId == todoId
                                then
                                    { todo | done = not todo.done}
                            else
                                todo
                        updatedTodos = List.map (toggleDone todoId) model.todosRpl
                    in
                        ( { model | todosRpl = updatedTodos }, Cmd.none )
                Err error ->
                    let _ = Debug.log "Msgs.OnTodoDoneUpdate Failure:" error in
                    ( model, Cmd.none )
        Msgs.InputNewTodo todo ->
            ( { model | newTodo = todo }, Cmd.none )
        Msgs.CreateNewTodo ->
            let
                newTodo = { todoId = 0
                          , title = model.newTodo
                          , done = False
                          }
            in
                ( model, createNewTodo newTodo )
        Msgs.OnCreateNewTodo result ->
            case result of
                RemoteData.Success newTodo ->
                    let
                        newModel =
                            { model | todosRpl = model.todosRpl ++ [newTodo] }
                    --( { model | todosRpl = model.todosRpl ++ [newTodo] }
                    --, Cmd.none
                    --)
                    in
                        update Msgs.FocusNewTodoTextfield newModel
                _ ->
                    let _ = Debug.log "Msgs.OnCreateNewTodo:" "NotAsked, Loading, or Failure" in
                    ( model, Cmd.none )
        Msgs.FocusNewTodoTextfield ->
            ( model
            , Task.attempt (\_->Msgs.NoOp) <| Dom.focus "new-todo-textfield"
            )


        Msgs.NoOp ->
            ( model, Cmd.none )
