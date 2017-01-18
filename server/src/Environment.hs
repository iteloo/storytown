module Environment where

import           System.Environment
import           Text.Read          (readMaybe)

data Environment = Development | Test | Production
  deriving (Show, Read)

environmentEnvVar = "ENVIRONMENT"

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
