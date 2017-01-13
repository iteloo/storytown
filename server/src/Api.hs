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
import           Servant.Elm
import Servant.Auth.Server


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DItem
    text String
    deriving Show
|]


type ItemId = Int64

data Item
  = Item {
    id   :: ItemId,
    text :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON Item
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

type Protected
   = "name" :> Get '[JSON] String
 :<|> "email" :> Get '[JSON] String

type Unprotected =
      Raw
 -- :<|> "login"
 --     :> ReqBody '[JSON] Login
 --     :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)

type API auths =
       (Auth auths User :> Protected)
  :<|> Unprotected

type ItemApi =
  "api" :> (
         "item" :> Get '[JSON] [ItemId]
    :<|> "item" :> Capture "itemId" ItemId :> Get '[JSON] Item
    :<|> "item" :> ReqBody '[JSON] String :> Post '[JSON] ItemId
    :<|> "item" :> Capture "itemId" ItemId :> Delete '[JSON] NoContent

  )

itemApi :: Proxy ItemApi
itemApi = Proxy
