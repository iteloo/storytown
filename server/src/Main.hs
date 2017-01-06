
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.IO

import           App

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Database.Persist.Postgresql
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Options.Applicative
import           Web.Heroku.Persist.Postgresql        (fromDatabaseUrl)



main :: IO ()
main = do
  opts <- execParser $ info (helper <*> appSettings)
      ( fullDesc
     <> progDesc "Runs storytown server"
     <> header "storytown - build your language adventure" )
  let p = port opts
      settings =
        setPort p $
        setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " ++ show p ++ "...")) $
        defaultSettings
  db_url <- lookupEnv "DATABASE_URL"
  case db_url of
    Nothing -> fail "No DATABASE_URL"
    Just url -> do
      let pgconf = fromDatabaseUrl 1 url
      -- hPutStrLn stderr (show $ pgConnStr pgconf)
      runStdoutLoggingT
        $ withPostgresqlPool (pgConnStr pgconf) (pgPoolSize pgconf)
        $ \pool -> liftIO $ do
            let cfg = Config pool
            runSettings settings . logStdoutDev =<< startApp cfg

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
