{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Data.Aeson
import           Data.Int            (Int64)
import           Data.Proxy
import           Database.Persist
import           Database.Persist.TH
import           GHC.Generics
import           Servant.API
import           Servant.Auth.Server
import           Servant.Elm

-- DATA MODELS

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DItem
    text String
    audioUrl String Maybe
    deriving Show
|]

type ItemId = Int64

data Item
  = Item {
    idKey    :: ItemId,
    text     :: String,
    audioUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item
instance ElmType Item

data User = User { name :: String, email :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data Login = Login { username :: String, password :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login
instance ElmType Login

-- API

type API auths =
       SubAPI auths
  :<|> Raw

type SubAPI auths =
    (Auth auths User :> "api" :> Protected)
  :<|> Unprotected

type Protected =
       "name" :> Get '[JSON] String
  :<|> "email" :> Get '[JSON] String
  :<|> "item" :> ItemApi
  :<|> "s3" :> S3Api

type S3Api =
  Capture "dir" String :> Get '[JSON] String

type Unprotected =
  "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] String

type ItemApi =
       Get '[JSON] [ItemId]
  :<|> Capture "itemId" ItemId :> Get '[JSON] Item
  :<|> ReqBody '[JSON] String :> Post '[JSON] ItemId
  :<|> ReqBody '[JSON] Item :> Put '[JSON] ItemId
  :<|> Capture "itemId" ItemId :> Delete '[JSON] NoContent
