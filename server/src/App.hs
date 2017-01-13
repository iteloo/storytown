{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           System.IO
-- import           Network.Wai
-- import           Network.Wai.MakeAssets
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Servant
import           Servant.Auth.Server

import           Api


data Config = Config {
    pool :: DB.ConnectionPool
  , env  :: Environment
}

data Environment = Development | Test | Production
  deriving (Show, Read)

startApp :: Config -> IO Application
startApp cfg = do
  -- We generate the key for signing tokens. This would generally be persisted,
  -- and kept safely
  myKey <- generateKey
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtCfg = defaultJWTSettings myKey
      ctx = defaultCookieSettings :. jwtCfg :. EmptyContext
      --- Here we actually make concrete
      api = Proxy :: Proxy (API '[JWT])
  _ <- makeToken jwtCfg "Leo" "iteloo@gmail.com"
  return $ serveWithContext api ctx (server defaultCookieSettings jwtCfg)

  -- run migrations
  -- DB.runSqlPool (DB.runMigration Api.migrateAll) (pool cfg)
  -- return $ app cfg

startTokenCreation jwtCfg = do
  hPutStrLn stderr "Enter name and email separated by a space for a new token"
  forever $ do
     xs <- words <$> getLine
     case xs of
       [name', email'] -> makeToken jwtCfg name' email'
       _ -> hPutStrLn stderr "Expecting a name and email separated by spaces"

makeToken jwtCfg name' email' = do
  etoken <- makeJWT (User name' email') jwtCfg Nothing
  case etoken of
    Left e -> hPutStrLn stderr $ "Error generating token:t" ++ show e
    Right v -> hPutStrLn stderr $ "New token:\t" ++ show v

-- app :: Config -> Application
-- app cfg =
--   serve api (enter (myHandlerToHandler cfg) (server cfg) :<|> serveDirectory "assets")

type Api = ItemApi :<|> Raw

api :: Proxy Api
api = Proxy

type MyHandler = ReaderT Config Handler

myHandlerToHandler :: Config -> MyHandler :~> Handler
myHandlerToHandler cfg = Nat $ \x -> runReaderT x cfg

-- server :: Config -> ServerT ItemApi MyHandler
-- server cfg =
--        listItems
--   :<|> getItem
--   :<|> postItem
--   :<|> deleteItem

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

deleteItem :: ItemId -> MyHandler NoContent
deleteItem itemid = fmap (\() -> NoContent) $ runDb
    $ DB.delete (DB.toSqlKey itemid :: Key DItem)

-- AUTH

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: AuthResult User -> Server Protected
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected (Authenticated user) = return (name user) :<|> return (email user)
-- Otherwise, we return a 401.
protected _ = throwAll err401


unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts =
       serveDirectory "example/static"
  -- :<|> checkCreds cs jwts


server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts = protected :<|> unprotected cs jwts
