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
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Database.Persist
import           Database.Persist.TH
import           GHC.Generics
import           Servant.API
import           Servant.Auth.Server
import           Servant.Elm

-- DATA MODELS

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DUser
    firstName Text
    lastName Text
    email String
    password String
    group String
    deriving Show
DStory
    title Text
    deriving Show
DItem
    storyId DStoryId
    text Text
    audioUrl String Maybe
    deriving Show
|]

type UserId = Int64
type StoryId = Int64
type ItemId = Int64

data Item = Item {
    text     :: Text,
    audioUrl :: Maybe String
} deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item
instance ElmType Item

data User = User {
    userId    :: UserId
  , firstName :: Text
  , lastName  :: Text
  , email     :: String
  , group     :: UserGroupUnsafe
} deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User
instance ElmType User

data Story = Story {
    title     :: Text
  , sentences :: [Item]
} deriving (Eq, Show, Generic)

instance ToJSON Story
instance FromJSON Story
instance ElmType Story

data Login = Login {
    username :: String
  , password :: String
} deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login
instance ElmType Login

data AuthData = AuthData {
    user :: User
} deriving (Eq, Show, Read, Generic)

instance ToJSON AuthData
instance FromJSON AuthData
instance ElmType AuthData

type UserGroupUnsafe = String

data UserGroup =
    Teacher
  | Student
  deriving (Eq, Show, Read, Generic)

userGroupToUnsafe :: UserGroup -> UserGroupUnsafe
userGroupToUnsafe = show

-- API

type API auths =
       SubAPI auths
  :<|> Raw

type SubAPI auths =
    (Auth auths User :> "api" :> Protected)
  :<|> Unprotected

type Protected =
       "name" :> Get '[JSON] Text
  :<|> "email" :> Get '[JSON] String
  :<|> "story" :> StoryApi
  :<|> "s3" :> S3Api

type S3Api =
  Capture "dir" String :> Get '[JSON] String

type Unprotected =
  "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthData)

type StoryApi =
       Get '[JSON] [(StoryId, Story)]
  :<|> ReqBody '[JSON] Story :> Post '[JSON] StoryId
  :<|> Capture "id" StoryId :> Get '[JSON] Story
  :<|> Capture "id" StoryId :> ReqBody '[JSON] Story :> Put '[JSON] NoContent
  :<|> Capture "id" StoryId :> Delete '[JSON] NoContent
