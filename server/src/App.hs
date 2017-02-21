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
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Time                           as Time
import qualified Data.Time.Clock.POSIX               as Time
import           Database.Persist                    ((==.))
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
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      ctx = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])

  DB.runSqlPool (DB.runMigration Api.migrateAll) (pool cfg)
  let corsPolicy = Cors.simpleCorsResourcePolicy {
          Cors.corsRequestHeaders = Cors.simpleHeaders ++ [ hAuthorization ]
        , Cors.corsMethods = Cors.simpleMethods ++ [ "DELETE" ]
        }
  e <- newEnv Discover <&> set envRegion Oregon
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
       return (firstName user)
  :<|> return (email user)
  -- :<|> itemServer
  :<|> storyServer
  :<|> getSignedPutObjectRequest
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> MyServer Unprotected
unprotected = checkCreds

checkCreds :: CookieSettings -> JWTSettings -> Login -> MyHandler AuthData
checkCreds cookieSettings jwtSettings (Login "" "") = do
   let usr = User {
     userId = 0
   , firstName = "Ali"
   , lastName = "Baba"
   , email = "ali@email.com"
   , group = "Teacher"
   }
   etoken <- liftIO $ makeJWT usr jwtSettings Nothing
   case etoken of
     Left e     -> throwError err401
     Right token -> return AuthData {
        jwt = LBS.unpack token
      , user = usr
    }
checkCreds _ _ _ = throwError err401


-- S3 signing

getSignedPutObjectRequest :: String -> MyHandler String
getSignedPutObjectRequest dir = do
  ts <- liftIO Time.getCurrentTime
  let posix = show $ fromEnum $ Time.utcTimeToPOSIXSeconds ts
  let objectkey = ObjectKey $ Text.pack $ dir ++ "/" ++ posix ++ ".ogg"
  -- [hack] uploads some random file, to be overwritten by actual
  b <- liftIO $ chunkedFile 2048 "stack.yaml"
  let req = putObject "storytown-bucket" objectkey b
  -- [todo] allow client to pass in content-type
              & poContentType .~ Just "audio/ogg"
  lift (BS.unpack <$> presignURL ts 1200 req)


-- DB HANDLER

runDb query = asks pool >>= liftIO . DB.runSqlPool query


-- STORY HANDLERS

storyServer :: MyServer StoryApi
storyServer =
       getStories
  :<|> postStory
  :<|> getStory
  :<|> putStory
  :<|> deleteStory

getStories :: MyHandler [(StoryId, Story)]
getStories = do
    ids <- runDb $ DB.selectKeysList [] [] :: MyHandler [Key DStory]
    sequence [ do
        story <- getStory storyId
        return (storyId, story)
      | storyId <- DB.fromSqlKey <$> ids ]

postStory :: Story -> MyHandler StoryId
postStory story = runDb $ do
    storyId <- DB.fromSqlKey <$> DB.insert DStory {
        dStoryTitle = title story
      }
    insertSentencesForStory storyId story
    return storyId

getStory :: StoryId -> MyHandler Story
getStory newId = runDb $ do
      let storyId = DB.toSqlKey newId
      mstory <- DB.get storyId
      case mstory of
        Nothing -> fail "cannot get story"
        Just story -> do
          -- get all sentences
          sentences <- DB.selectList [DItemStoryId ==. storyId] []
          return Story {
              title = dStoryTitle story
            , sentences = [ Item {
                text = dItemText (DB.entityVal s)
              , audioUrl = dItemAudioUrl (DB.entityVal s)
              } | s <- sentences ]
            }

putStory :: StoryId -> Story -> MyHandler NoContent
putStory storyId story = runDb $ do
    DB.repsert (DB.toSqlKey storyId) DStory {
        dStoryTitle = title story
      }
    -- remove sentences under this storyid
    deleteAllSentences storyId
    -- insert new sentences
    insertSentencesForStory storyId story
    return NoContent

deleteStory :: StoryId -> MyHandler NoContent
deleteStory storyid = runDb $ do
    DB.delete (DB.toSqlKey storyid :: Key DStory)
    deleteAllSentences storyid
    return NoContent

insertSentencesForStory storyId story =
    sequence_ [
      DB.insert_ DItem {
          dItemStoryId = DB.toSqlKey storyId
        , dItemText = text s
        , dItemAudioUrl = audioUrl s
        }
      | s <- sentences story ]

deleteAllSentences storyid =
    DB.deleteWhere [DItemStoryId ==. DB.toSqlKey storyid]
