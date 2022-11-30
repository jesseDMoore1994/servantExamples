{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Https (main) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant

type API = Get '[JSON] Int

api :: Proxy API
api = Proxy

server :: Server API
server = return 10

app :: Application
app = serve api server

main :: IO ()
main = runTLS tlsOpts warpOpts app
  where tlsOpts = tlsSettings "./ssl/certificate.pem" "./ssl/key.pem"
        warpOpts = setPort 8080 defaultSettings
