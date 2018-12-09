{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Proxy
import Elm
import Servant.Elm
import Shelly (mkdir_p, shelly)
import Todo (CRUD, Todo)

elmOpts :: ElmOptions
elmOpts =
    defElmOptions
        { urlPrefix = Static "http://localhost:8081"
        }

spec :: Spec
spec = Spec ["Generated", "TodoAPI"]
    (defElmImports
        : toElmTypeSource (Proxy :: Proxy Todo)
        : toElmDecoderSource (Proxy :: Proxy Todo)
        : toElmEncoderSource (Proxy :: Proxy Todo)
        : generateElmForAPIWith elmOpts (Proxy :: Proxy CRUD)
        )

main :: IO ()
main = do
    shelly $ mkdir_p "simple-todo-app/src/Generated"
    specsToDir [spec] "simple-todo-app/src"
