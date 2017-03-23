import           Network.Wai.Handler.Warp
import           System.IO

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import qualified Data.ByteString.Char8                as BS
import           Data.Semigroup                       ((<>))
import           Database.Persist.Postgresql
import qualified Network.Wai.Handler.WarpTLS          as TLS
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Options.Applicative
import           Web.Heroku.Persist.Postgresql        (fromDatabaseUrl)

import           App
import           Environment

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> appSettings)
      ( fullDesc
     <> progDesc "Runs storytown server"
     <> header "storytown - build your language adventure" )
  env <- readEnvWithDefault Development environmentEnvVar
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
          setPort p
        $ setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " ++ show p ++ "..."))
          defaultSettings
  let tls = TLS.tlsSettings "ssl/certificate.pem" "ssl/key.pem"
  runStdoutLoggingT
    $ withPostgresqlPool (pgConnStr pgconf) (pgPoolSize pgconf)
    $ \pool -> liftIO $ do
        let cfg = Config pool env
        TLS.runTLS tls settings . logger =<< startApp cfg

newtype AppSettings = AppSettings {
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
