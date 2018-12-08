module Models exposing (..)

import Generated.TodoAPI exposing (Todo)
import Material
import RemoteData exposing (WebData)

type alias Model =
    { todos : WebData (List Todo)
    , todosRpl : List Todo
    , mdl : Material.Model
    , newTodo : String
    }

initialModel : Model
initialModel =
    { todos = RemoteData.Loading
    , todosRpl = []
    , mdl = Material.model
    , newTodo = ""
    }
