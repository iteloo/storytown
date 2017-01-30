{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module App where

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource        (ResourceT)
import qualified Data.ByteString.Char8               as BS
import qualified Data.ByteString.Lazy.Char8          as LBS
import qualified Data.Text                           as Text
import qualified Data.Time                           as Time
import qualified Data.Time.Clock.POSIX               as Time
import qualified Database.Persist                    as DB
import qualified Database.Persist.Postgresql         as DB
import           Network.AWS.S3
import           Network.HTTP.Types.Header           (hAuthorization)
import qualified Network.Wai.Middleware.Cors         as Cors
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           System.IO                           (stdout)

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
  e <- newEnv Oregon Discover
  l <- newLogger Debug stdout
  return
    $ Cors.cors (const $ Just corsPolicy)
    $ serveWithContext api ctx
    $ enter (myHandlerToHandler cfg (e & envLogger .~ l))
        (server defaultCookieSettings jwtCfg)
      :<|> serveDirectory "assets"

type MyHandler = ReaderT Config (AWST (ResourceT Handler))

type MyServer api = ServerT api MyHandler

myHandlerToHandler :: Config -> Env -> MyHandler :~> Handler
myHandlerToHandler cfg env =
  Nat $ runResourceT . runAWST env . flip runReaderT cfg

server :: CookieSettings -> JWTSettings -> MyServer (SubAPI auths)
server cs jwts =
  protected :<|> unprotected cs jwts


-- AUTH

protected :: AuthResult User -> MyServer Protected
protected (Authenticated user) =
       return (name user)
  :<|> return (email user)
  :<|> itemServer
  :<|> getSignedPutObjectRequest
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> MyServer Unprotected
unprotected = checkCreds

checkCreds :: CookieSettings -> JWTSettings -> Login -> MyHandler String
checkCreds cookieSettings jwtSettings (Login "" "") = do
   let usr = User "Ali Baba" "ali@email.com"
   etoken <- liftIO $ makeJWT usr jwtSettings Nothing
   case etoken of
     Left e     -> throwError err401
     Right token -> return $ LBS.unpack token
checkCreds _ _ _ = throwError err401


-- S3 signing

getSignedPutObjectRequest :: String -> MyHandler String
getSignedPutObjectRequest dir = do
  ts <- liftIO Time.getCurrentTime
  let posix = show $ fromEnum $ Time.utcTimeToPOSIXSeconds ts
  let objectkey = ObjectKey $ Text.pack $ dir ++ "/" ++ posix
  -- [hack] uploads some random file, to be overwritten by actual
  b <- liftIO $ chunkedFile 2048 "stack.yaml"
  let req = putObject "storytown-bucket" objectkey b
  -- [todo] allow client to pass in content-type
              & poContentType .~ Just "audio/ogg"
  lift $ fmap BS.unpack $ presignURL ts 1200 req


-- ITEM HANDLERS

itemServer :: MyServer ItemApi
itemServer =
       listItems
  :<|> getItem
  :<|> postItem
  :<|> putItem
  :<|> deleteItem

runDb query = asks pool >>= liftIO . DB.runSqlPool query

listItems :: MyHandler [ItemId]
listItems = fmap (map DB.fromSqlKey)
    (runDb $ DB.selectKeysList [] [] :: MyHandler [Key DItem])

getItem :: ItemId -> MyHandler Item
getItem newId = fmap (dItemToItem newId) $ runDb $ do
      item <- DB.get $ DB.toSqlKey newId
      maybe (fail "cannot get") return item

postItem :: String -> MyHandler ItemId
postItem new = fmap DB.fromSqlKey $ runDb
    $ DB.insert (DItem new Nothing)

putItem :: Item -> MyHandler ItemId
putItem item = runDb $ do
    -- [problem] will insert if new id
      DB.repsert (DB.toSqlKey (idKey item)) (itemToDItem item)
      return $ idKey item

deleteItem :: ItemId -> MyHandler NoContent
deleteItem itemid = fmap (\() -> NoContent) $ runDb
    $ DB.delete (DB.toSqlKey itemid :: Key DItem)

itemToDItem (Item id txt url) = DItem txt url

dItemToItem id (DItem txt url) = Item id txt url
