module Main where

import Lib
import Resource.Todos (DbConf(..), conf)
import qualified Resource.Todos as Todos
import qualified Resource.Users as Users

main :: IO ()
main = Todos.initDB (db conf) (table conf)
       >> Users.initDB (db conf) Users.tableName
       >> startApp
