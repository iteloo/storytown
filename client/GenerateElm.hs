{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Elm                                 (ElmType (..), Spec (Spec),
                                                      specsToDir,
                                                      toElmDecoderSource,
                                                      toElmEncoderSource,
                                                      toElmTypeSource)
import           GHC.Generics                        (Generic)
import           GHC.TypeLits
import           Servant.API
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Elm                         (ElmOptions (..), ElmType,
                                                      Proxy (Proxy),
                                                      UrlPrefix (..),
                                                      defElmImports,
                                                      defElmOptions,
                                                      generateElmForAPIWith)
import           Servant.Elm.Internal.Foreign
import           Servant.Foreign


import           Api
import           Environment

spec :: ElmOptions -> Spec
spec opt =
  Spec ["Api"]
            (defElmImports
             :  toElmTypeSource    (Proxy :: Proxy NoContent)
             :  toElmSource        (Proxy :: Proxy Login)
             ++ toElmSource        (Proxy :: Proxy User)
             ++ toElmSource        (Proxy :: Proxy AuthData)
             ++ toElmSource        (Proxy :: Proxy Story)
             ++ toElmSource        (Proxy :: Proxy Item)
             ++ generateElmForAPIWith opt (Proxy :: Proxy (SubAPI auths)))

main :: IO ()
main = do
  env <- readEnvWithDefault Development environmentEnvVar
  let localOptions = defElmOptions
        { urlPrefix = Servant.Elm.Static "http://localhost:5000" }
  let opt = case env of
          Development -> localOptions
          Test        -> localOptions
          Production  -> defElmOptions
  specsToDir [spec opt] "client"

toElmSource (proxy :: Proxy a) =
  [ toElmTypeSource    proxy
  , toElmEncoderSource proxy
  , toElmDecoderSource proxy ]

-- HACK

instance (HasForeign lang ftype sublayout)
    => HasForeign lang ftype (Auth auths a :> sublayout) where
  type Foreign ftype (Auth auths a :> sublayout) = Foreign ftype sublayout
  foreignFor lang ftype Proxy =
    foreignFor lang ftype (Proxy :: Proxy sublayout)
