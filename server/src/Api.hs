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


type ItemApi =
  "api" :> (
         "item" :> Get '[JSON] [ItemId]
    :<|> "item" :> Capture "itemId" ItemId :> Get '[JSON] Item
    :<|> "item" :> ReqBody '[JSON] String :> Post '[JSON] ItemId
    :<|> "item" :> Capture "itemId" ItemId :> Delete '[JSON] ()
  )

itemApi :: Proxy ItemApi
itemApi = Proxy
