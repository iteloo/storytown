{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
-- import           Network.Wai
-- import           Network.Wai.MakeAssets
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Servant

import           Api


data Config = Config {
  pool :: DB.ConnectionPool
}

startApp :: Config -> IO Application
startApp cfg = do
  DB.runSqlPool (DB.runMigration Api.migrateAll) (pool cfg)
  return $ app cfg

app :: Config -> Application
app cfg =
  serve api (enter (myHandlerToHandler cfg) (server cfg) :<|> serveDirectory "assets")

type Api = ItemApi :<|> Raw

api :: Proxy Api
api = Proxy

type MyHandler = ReaderT Config Handler

myHandlerToHandler :: Config -> MyHandler :~> Handler
myHandlerToHandler cfg = Nat $ \x -> runReaderT x cfg

server :: Config -> ServerT ItemApi MyHandler
server cfg =
       listItems
  :<|> getItem
  :<|> postItem
  :<|> deleteItem

runDb query = asks pool >>= liftIO . DB.runSqlPool query

listItems :: MyHandler [ItemId]
listItems = fmap (map DB.fromSqlKey)
    (runDb $ DB.selectKeysList [] [] :: MyHandler [Key DItem])

getItem :: ItemId -> MyHandler Item
getItem new = fmap (\(DItem txt) -> Item new txt) $ runDb $ do
      item <- DB.get $ DB.toSqlKey new
      maybe (fail "cannot get") return item

postItem :: String -> MyHandler ItemId
postItem new =  fmap DB.fromSqlKey $ runDb
    $ DB.insert (DItem new)

deleteItem :: ItemId -> MyHandler ()
deleteItem itemid = runDb
    $ DB.delete (DB.toSqlKey itemid :: Key DItem)
