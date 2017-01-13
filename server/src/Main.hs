
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.IO

import           App

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import qualified Data.ByteString.Char8                as BS
import           Database.Persist.Postgresql
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Options.Applicative
import           Text.Read                            (readMaybe)
import           Web.Heroku.Persist.Postgresql        (fromDatabaseUrl)


main :: IO ()
main = do
  opts <- execParser $ info (helper <*> appSettings)
      ( fullDesc
     <> progDesc "Runs storytown server"
     <> header "storytown - build your language adventure" )
  env <- readEnvWithDefault Development "ENVIRONMENT"
  url <- unsafeLookupEnv "DATABASE_URL"
  let pgconf = case env of
          Development -> PostgresConf (BS.pack url) 1
          Test        -> PostgresConf (BS.pack "") 1
          Production  -> fromDatabaseUrl 1 url
  let logger = case env of
          Development -> logStdoutDev
          Test        -> logStdoutDev
          Production  -> logStdoutDev  -- [todo] disable in future
  let p = port opts
      settings =
        setPort p $
        setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " ++ show p ++ "...")) $
        defaultSettings
  runStdoutLoggingT
    $ withPostgresqlPool (pgConnStr pgconf) (pgPoolSize pgconf)
    $ \pool -> liftIO $ do
        let cfg = Config pool env
        runSettings settings . logger =<< startApp cfg

data AppSettings = AppSettings {
  port :: Int
}

appSettings :: Parser AppSettings
appSettings = AppSettings
     <$> option auto
         ( long "port"
        <> short 'p'
        <> help "Port to host the server"
        <> showDefault
        <> value 3000
        <> metavar "INT" )


-- HELPERS

unsafeReadEnv :: Read a => String -> IO a
unsafeReadEnv env =
  maybe
    (fail $ "No " ++ env ++ " found!")
    (maybe
      (fail $ "Cannot read " ++ env ++ "!")
      return
      . readMaybe)
  =<< lookupEnv env

readEnvWithDefault :: Read a => a -> String -> IO a
readEnvWithDefault def = fmap (maybe def id . (>>= readMaybe))  . lookupEnv

unsafeLookupEnv env = lookupEnv env
    >>= maybe (fail $ "No " ++ env ++ " found!") return
