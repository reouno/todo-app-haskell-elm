module Main where

import Lib
import Resource.Todos (DbConf(..), conf, initDB)

main :: IO ()
main = initDB (db conf) (table conf)
       >> startApp
