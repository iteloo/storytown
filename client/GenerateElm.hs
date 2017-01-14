{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Api
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
import           Servant.Elm                         (ElmType, Proxy (Proxy),
                                                      defElmImports,
                                                      generateElmForAPI)
import           Servant.Elm.Internal.Foreign
import           Servant.Foreign


spec :: Spec
spec = Spec ["Api"]
            (defElmImports
             :  toElmTypeSource    (Proxy :: Proxy NoContent)
             :  toElmSource        (Proxy :: Proxy Login)
             ++ toElmSource        (Proxy :: Proxy Item)
             ++ generateElmForAPI  (Proxy :: Proxy (SubAPI auths)))

main :: IO ()
main = specsToDir [spec] "client"

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
