module Resource.Users
    ( tableName
    , initDB
    , insert
    , selectAll
    , select
    , update
    , delete
    ) where

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
import Resource.Todos (DbConf(..), conf)
import User

tableName :: String
tableName = "users"

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
            ++ " (userId INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, email TEXT, passHash TEXT)"
            )
    in
        execute_ conn qstr

insert :: FilePath -> User -> IO User
insert dbPath row =
    withConnection dbPath $ \conn ->
        insert_ conn tableName row

insert_ :: Connection -> String -> User -> IO User
insert_ conn table row = do
    let
        qstr = fromString ("INSERT INTO "
                    ++ table
                    ++ " (name, email, passHash) "
                    ++ "VALUES (?, ?, ?)") :: Query
        qstr2 = fromString ("SELECT * FROM "
                    ++ table
                    ++ " WHERE userId = last_insert_rowid()") :: Query
    execute conn qstr ( name row :: String
                      , email row :: String
                      , passHash row :: String
                      )
    rows <- query_ conn qstr2
    return $ head rows

selectAll :: FilePath -> IO [User]
selectAll dbPath =
    withConnection dbPath $ \conn ->
        selectAll_ conn tableName

selectAll_ :: Connection -> String -> IO [User]
selectAll_ conn table =
    let
        qstr = fromString ("SELECT * from " ++ table)
    in
        query_ conn qstr :: IO [User]

select :: FilePath -> Int -> IO User
select dbPath id' =
    withConnection dbPath $ \conn ->
        select_ conn tableName id'

select_ :: Connection -> String -> Int -> IO User
select_ conn table id' = do
    let
        qstr = fromString (  "SELECT * from "
                          ++ table
                          ++ " where userId = (?)"
                          ) :: Query
    rows <- query conn qstr (Only (id' :: Int))
    return $ head rows

update :: FilePath -> User -> IO ()
update dbPath row =
    withConnection dbPath $ \conn ->
        update_ conn tableName row

update_ :: Connection -> String -> User -> IO ()
update_ conn table row =
    execute conn (fromString ("UPDATE "
              ++ table
              ++ " SET name = (?), email = (?), passHash = (?) WHERE userId = (?)"))
        ( name row :: String
        , email row :: String
        , passHash row :: String
        , userId row :: Int
        )

delete :: FilePath -> Int -> IO ()
delete dbPath id' =
    withConnection dbPath $ \conn ->
        delete_ conn tableName id'

delete_ :: Connection -> String -> Int -> IO ()
delete_ conn table id' =
    execute conn (fromString ("DELETE from "
               ++ table
               ++ " WHERE userId = (?)"))
        (Only (id' :: Int))
