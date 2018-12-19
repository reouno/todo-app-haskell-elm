{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Todo where

import Control.Applicative
import Data.Aeson
import Data.Proxy
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import GHC.Generics
import Servant.API
import Servant.Elm (ElmType)
import Web.Internal.FormUrlEncoded (FromForm)

data Todo = Todo
    { todoId :: Int
    , title :: String
    , done :: Bool
    } deriving (Generic, Show)

instance FromJSON Todo
instance ToJSON Todo
instance FromForm Todo
instance ElmType Todo
instance FromRow Todo where
    fromRow = Todo <$> field <*> field <*> field

type GetTodos = "todos" :> Get '[JSON] [Todo]
type PostTodo = "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
type PutTodo = "todos" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
type DeleteTodo = "todos" :> Capture "id" Int :> Delete '[JSON] ()
