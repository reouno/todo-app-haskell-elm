{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module User where

import Data.Aeson (FromJSON, ToJSON)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import GHC.Generics (Generic)
import Servant.API ( (:<|>), (:>), JSON
                   , Capture, Delete, Get, FormUrlEncoded, Post, Put, ReqBody)
import Servant.Elm (ElmType)
import Web.Internal.FormUrlEncoded (FromForm)

data User = User
    { userId :: Int
    , name :: String
    , email :: String
    , passHash :: String
    } deriving (Eq, Generic, Show)

instance FromJSON User
instance ToJSON User
instance FromForm User -- derive "fromForm" parsing Form into value
instance ElmType User
instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field

type GetUsers = "users" :> Get '[JSON] [User]
type GetUser = "users" :> Capture "id" Int :> Get '[JSON] User
type PostUser = "users" :> ReqBody '[JSON, FormUrlEncoded] User :> Post '[JSON] User
type PutUser = "users" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] User :> Put '[JSON] ()
type DeleteUser = "users" :> Capture "id" Int :> Delete '[JSON] ()
