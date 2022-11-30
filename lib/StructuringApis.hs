{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module StructuringApis (serve_api) where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant

type API = SimpleAPI "users" User UserId
      :<|> SimpleAPI "products" Product ProductId


type SimpleAPI (name :: Symbol) a i = name :>
  (                         Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] a
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )

simpleServer
  :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (SimpleAPI name a i)
simpleServer listAs getA postA =
  listAs :<|> getA :<|> postA

userServer :: Server (SimpleAPI "users" User UserId)
userServer = simpleServer
  (return [])
  (\userid -> return $
      if userid == 0
      then User "john" 64
      else User "everybody else" 10
  )
  (\_user -> return NoContent)

productServer :: Server (SimpleAPI "products" Product ProductId)
productServer = simpleServer
  (return [])
  (\_productid -> return $ Product "Great stuff")
  (\_product -> return NoContent)

type UserId = Int

data User = User { username :: String, age :: Int }
  deriving Generic

instance FromJSON User
instance ToJSON   User

type ProductId = Int

data Product = Product { productname :: String }
  deriving Generic

instance FromJSON Product
instance ToJSON   Product

api :: Proxy API
api = Proxy

serve_api :: IO ()
serve_api = run 8080 . serve api $
  userServer :<|> productServer
