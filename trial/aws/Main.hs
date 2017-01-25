{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Time
import           Network.AWS
import           Network.AWS.S3
import           System.IO

main :: IO ()
main = do
    -- To specify configuration preferences, newEnv is used to create a new Env. The Region denotes the AWS region requests will be performed against,
    -- and Credentials is used to specify the desired mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case, Discover will cause the library to try a number of options such as default environment variables, or an instance's IAM Profile:
    e <- newEnv Oregon Discover

    -- A new Logger to replace the default noop logger is created, with the logger set to print debug information and errors to stdout:
    l <- newLogger Debug stdout

    -- The payload (and hash) for the S3 object is retrieved from a FilePath:
    b <- chunkedFile 2048 "stack.yaml"

    ts  <- getCurrentTime

    -- We now run the AWS computation with the overriden logger, performing the PutObject request:
    let req = putObject "storytown-bucket" "hhhh" b
    _ <- runResourceT . runAWS (e & envLogger .~ l) $ do
      -- send req
      url <- presignURL ts 60 req
      liftIO $ print url
    return ()
