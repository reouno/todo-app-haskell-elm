{-# LANGUAGE OverloadedStrings #-}
module Resource.Todos
    ( DbConf(..)
    , conf
    , delete
    , initDB
    , insert
    , selectAll
    , update
    ) where

import Control.Applicative
import Data.String (fromString)
import Database.SQLite.Simple ( Connection
                              , Query
                              , close
                              , execute
                              , execute_
                              , open
                              , query
                              , query_
                              , withConnection
                              )
import Database.SQLite.Simple.Types (Only(..))
import Todo

data DbConf = DbConf
    { db :: String
    , table :: String
    }

conf = DbConf
    { db = "../haskell-elm01-sample-dev.db"
    , table = "todos"
    }

initDB :: FilePath -> String -> IO ()
initDB dbPath table =
    withConnection dbPath $ \conn ->
        createTable conn table

createTable :: Connection -> String -> IO ()
createTable conn table =
    let
        qstr = fromString
            (  "CREATE TABLE IF NOT EXISTS "
            ++ table
            ++ " (todoId INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, done Bool)"
            )
    in
        execute_ conn qstr

insert :: FilePath -> Todo -> IO Todo
insert dbPath todo =
    withConnection dbPath $ \conn ->
        insert_ conn (table conf) todo

insert_ :: Connection -> String -> Todo -> IO Todo
insert_ conn table todo = do
    let
        qstr = fromString ("INSERT INTO "
                    ++ table
                    ++ " (title, done) "
                    ++ "VALUES (?, ?)") :: Query
        qstr2 = fromString ("SELECT * FROM "
                    ++ table
                    ++ " WHERE todoId = last_insert_rowid()") :: Query
    execute conn qstr (title todo :: String, done todo :: Bool)
    rows <- query_ conn qstr2
    return $ head rows



selectAll :: FilePath -> IO [Todo]
selectAll dbPath =
    withConnection dbPath $ \conn ->
        selectAll_ conn $ table conf

selectAll_ :: Connection -> String -> IO [Todo]
selectAll_ conn table =
    let
        qstr = fromString ("SELECT * from " ++ table)
    in
        query_ conn qstr :: IO [Todo]

update :: FilePath -> Todo -> IO ()
update dbPath todo =
    withConnection dbPath $ \conn ->
        update_ conn (table conf) todo

update_ :: Connection -> String -> Todo -> IO ()
update_ conn table todo =
    execute conn (fromString ("UPDATE "
              ++ table
              ++ " SET title = (?), done = (?) WHERE todoId = (?)"))
        (title todo :: String, done todo :: Bool, todoId todo :: Int)

delete :: FilePath -> Int -> IO ()
delete dbPath id' =
    withConnection dbPath $ \conn ->
        delete_ conn (table conf) id'

delete_ :: Connection -> String -> Int -> IO ()
delete_ conn table id' =
    execute conn (fromString ("DELETE from "
               ++ table
               ++ " WHERE todoId = (?)"))
        (Only (id' :: Int))

--delete_All :: Connection -> String -> IO ()
--delete_All conn table =
--    execute_ conn $ fromString ("DELETE from " ++ table)
