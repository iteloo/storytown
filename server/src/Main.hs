
import           Network.Wai.Handler.Warp
import           System.IO

import           App

import Options.Applicative

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> appSettings)
      ( fullDesc
     <> progDesc "Runs storytown server"
     <> header "storytown - build your language adventure" )
  let p = port opts
      settings =
        setPort p $
        setBeforeMainLoop (hPutStrLn stderr
          ("listening on port " ++ show p ++ "...")) $
        defaultSettings
  runSettings settings =<< app


data AppSettings = AppSettings {
  port  :: Int
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
