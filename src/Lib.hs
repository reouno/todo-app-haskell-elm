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
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy, simpleMethods)
import Servant.API
import Servant.EDE (HTML, loadTemplates)
import Servant.Server
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Todo (Todo(..))
import qualified Todo

startApp :: IO ()
startApp = do
    db <- atomically $ newTVar (length initTodoList, IntMap.fromList initTodoList)
    _ <- loadTemplates api [] "."
    putStrLn "Listning at port 8081"
    Warp.run 8081 $ myCors $ serve api (server db)

myCors :: Middleware
myCors = cors (const $ Just policy)
    where
        policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            , corsMethods = "PUT" : simpleMethods
            }

type API
    = Get '[HTML "index.html"] Object
 :<|> "static" :> Raw
 :<|> Todo.CRUD

api :: Proxy API
api = Proxy

server :: TVar (Int, IntMap Todo) -> Server API
server db = index
    :<|> serveDirectoryFileServer "static"
    :<|> getTodos
    :<|> postTodo
    :<|> putTodoId
    :<|> deleteTodoId
    where
        index = pure mempty
        getTodos = liftIO $ IntMap.elems . snd <$> atomically (readTVar db)
        postTodo todo = liftIO . atomically $ do
            (maxId, m) <- readTVar db
            let
                newId = maxId + 1
                newTodo = todo { todoId = newId }
            writeTVar db (newId, IntMap.insert newId newTodo m)
            pure newTodo
        putTodoId tid todo =
            liftIO . atomically . modifyTVar db . second $ IntMap.insert tid todo
        deleteTodoId tid =
            liftIO . atomically . modifyTVar db . second $ IntMap.delete tid

initTodoList :: [(Int, Todo)]
initTodoList =
    [ (1, Todo 1 "release simple todo app" True)
    , (2, Todo 2 "pay the rent" False)
    , (3, Todo 3 "make money" False)
    , (4, Todo 4 "start a business" False)
    ]
