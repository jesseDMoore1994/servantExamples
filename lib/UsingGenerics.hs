{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module UsingGenerics (main, api, getLink, routesLinks, cliGet) where

import Control.Exception          (throwIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Network.Wai.Handler.Warp   (run)
import System.Environment         (getArgs)

import Servant
import Servant.Client

import Servant.API.Generic
import Servant.Client.Generic
import Servant.Server.Generic

data Routes route = Routes
    { _get :: route :- Capture "id" Int :> Get '[JSON] String
    , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
    }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

getLink :: Int -> Link
getLink = fieldLink _get

routesLinks :: Routes (AsLink Link)
routesLinks = allFieldLinks

cliRoutes :: Routes (AsClientT IO)
cliRoutes = genericClientHoist
    (\x -> runClientM x env >>= either throwIO return)
  where
    env = error "undefined environment"

cliGet :: Int -> IO String
cliGet = _get cliRoutes

record :: Routes AsServer
record = Routes
    { _get = return . show
    , _put = return . odd
    }

app :: Application
app = genericServe record

data AppCustomState =
  AppCustomState

type AppM = ReaderT AppCustomState Handler

--apiMyMonad :: Proxy (ToServantApi Routes)
--apiMyMonad = genericApi (Proxy :: Proxy Routes)

getRouteMyMonad :: Int -> AppM String
getRouteMyMonad = return . show

putRouteMyMonad :: Int -> AppM Bool
putRouteMyMonad = return . odd

recordMyMonad :: Routes (AsServerT AppM)
recordMyMonad = Routes {_get = getRouteMyMonad, _put = putRouteMyMonad}

-- natural transformation
nt :: AppCustomState -> AppM a -> Handler a
nt s x = runReaderT x s

appMyMonad :: AppCustomState -> Application
appMyMonad state = genericServeT (nt state) recordMyMonad

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            putStrLn "Starting cookbook-generic at http://localhost:8000"
            run 8000 app
        -- see this cookbook below for custom-monad explanation
        ("run-custom-monad":_) -> do
            putStrLn "Starting cookbook-generic with a custom monad at http://localhost:8000"
            run 8000 (appMyMonad AppCustomState)
        _ -> putStrLn "To run, pass 'run' argument: cabal new-run cookbook-generic run"
