{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Api
import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmEncoderSource, toElmTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON, NoContent)
import           Servant.Elm  (ElmType, Proxy (Proxy), defElmImports,
                               generateElmForAPI)


spec :: Spec
spec = Spec ["Api"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Item)
             : toElmDecoderSource (Proxy :: Proxy Item)
             : toElmEncoderSource (Proxy :: Proxy Item)
             : generateElmForAPI  (Proxy :: Proxy ItemApi))

main :: IO ()
main = specsToDir [spec] "client"
