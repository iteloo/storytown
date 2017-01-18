{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module App where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8          as BS
import qualified Database.Persist                    as DB
import qualified Database.Persist.Postgresql         as DB
import           Network.HTTP.Types.Header           (hAuthorization)
import qualified Network.Wai.Middleware.Cors         as Cors
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()

import           Api
import           Environment


data Config = Config {
    pool :: DB.ConnectionPool
  , env  :: Environment
}


startApp :: Config -> IO Application
startApp cfg = do
  -- We *also* need a key to sign the cookies
  myKey <- generateKey
  -- Adding some configurations. 'Cookie' requires, in addition to
  -- CookieSettings, JWTSettings (for signing), so everything is just as before
  let jwtCfg = defaultJWTSettings myKey
      ctx = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])

  DB.runSqlPool (DB.runMigration Api.migrateAll) (pool cfg)

  let corsPolicy = Cors.simpleCorsResourcePolicy {
          Cors.corsRequestHeaders = Cors.simpleHeaders ++ [ hAuthorization ]
        , Cors.corsMethods = Cors.simpleMethods ++ [ "DELETE" ]
        }
  return
    $ Cors.cors (const $ Just corsPolicy)
    $ serveWithContext api ctx
    $ (enter (myHandlerToHandler cfg)
        $ server defaultCookieSettings jwtCfg)
      :<|> serveDirectory "assets"

type MyHandler = ReaderT Config Handler

type MyServer api = ServerT api MyHandler

myHandlerToHandler :: Config -> MyHandler :~> Handler
myHandlerToHandler cfg = Nat $ \x -> runReaderT x cfg

server :: CookieSettings -> JWTSettings -> MyServer (SubAPI auths)
server cs jwts =
  protected :<|> unprotected cs jwts


-- AUTH

protected :: AuthResult User -> MyServer Protected
protected (Authenticated user) =
       return (name user)
  :<|> return (email user)
  :<|> itemServer
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> MyServer Unprotected
unprotected cs jwts =
       checkCreds cs jwts

checkCreds :: CookieSettings -> JWTSettings -> Login
  -> MyHandler String
checkCreds cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
   let usr = User "Ali Baba" "ali@email.com"
   etoken <- liftIO $ makeJWT usr jwtSettings Nothing
   case etoken of
     Left e     -> throwError err401
     Right token -> return $ BS.unpack token
checkCreds _ _ _ = throwError err401


-- ITEM HANDLERS

itemServer :: MyServer ItemApi
itemServer =
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

deleteItem :: ItemId -> MyHandler NoContent
deleteItem itemid = fmap (\() -> NoContent) $ runDb
    $ DB.delete (DB.toSqlKey itemid :: Key DItem)
