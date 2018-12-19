{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Control.Arrow (second)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Proxy (Proxy(..))
import Database.SQLite.Simple (fromOnly)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy, simpleMethods)
import Servant.API
import Servant.EDE (HTML, loadTemplates)
import Servant.Server
import Servant.Server.StaticFiles (serveDirectoryFileServer)

import Resource.Todos
import qualified Resource.Users as Users
import Todo (Todo(..))
import qualified Todo
import User (User(..))
import qualified User

startApp :: IO ()
startApp = do
    --db <- atomically $ newTVar (length initTodoList, IntMap.fromList initTodoList)
    _ <- loadTemplates api [] "."
    putStrLn "Listning at port 8081"
    Warp.run 8081 $ myCors $ serve api (server $ db conf)

myCors :: Middleware
myCors = cors (const $ Just policy)
    where
        policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            , corsMethods = "PUT" : simpleMethods
            }

--type API
--    = Get '[HTML "index.html"] Object
--    :<|> "static" :> Raw
--    :<|> Todo.CRUD
type API
    = Todo.GetTodos
    :<|> Todo.PostTodo
    :<|> Todo.PutTodo
    :<|> Todo.DeleteTodo
    :<|> User.GetUsers
    :<|> User.GetUser
    :<|> User.PostUser
    :<|> User.PutUser
    :<|> User.DeleteUser

api :: Proxy API
api = Proxy

server :: FilePath -> Server API
server dbPath = getTodos
    :<|> postTodo
    :<|> putTodoId
    :<|> deleteTodoId
    :<|> getUsers
    :<|> getUser
    :<|> postUser
    :<|> putUserId
    :<|> deleteUserId
    where
        getTodos :: Handler [Todo]
        getTodos =
            liftIO $ selectAll dbPath
        postTodo :: Todo -> Handler Todo
        postTodo todo =
            liftIO $ insert dbPath todo
            -- >> return NoContent

        -- id' is unused
        putTodoId :: Int -> Todo -> Handler ()
        putTodoId id' todo =
            liftIO $ update dbPath todo
            -- >> return NoContent
        deleteTodoId :: Int -> Handler ()
        deleteTodoId id' =
            liftIO $ delete dbPath id'
            -- >> return NoContent

        getUsers :: Handler [User]
        getUsers =
            liftIO $ Users.selectAll dbPath
        getUser :: Int -> Handler User
        getUser id' =
            liftIO $ Users.select dbPath id'
        postUser :: User -> Handler User
        postUser row =
            liftIO $ Users.insert dbPath row

        -- id' is unused
        putUserId :: Int -> User -> Handler ()
        putUserId id' row =
            liftIO $ Users.update dbPath row
            -- >> return NoContent
        deleteUserId :: Int -> Handler ()
        deleteUserId id' =
            liftIO $ Users.delete dbPath id'

initTodoList :: [(Int, Todo)]
initTodoList =
    [ (1, Todo 1 "release simple todo app" True)
    , (2, Todo 2 "pay the rent" False)
    , (3, Todo 3 "make money" False)
    , (4, Todo 4 "start a business" False)
    ]
