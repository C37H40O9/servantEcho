{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApp
  where


import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant
import Servant.API
import Servant.Client
import Servant.Server
import Network.Wai.Handler.Warp

import qualified Data.Text    as T
import qualified Data.Text.IO as T

type API = "echo" :> Capture "message" Text :> Get '[JSON] Message
      :<|> "sayHello"  :> Capture "name" Text :> Get '[JSON] Text

newtype Message = Message { msg :: Text }
  deriving Generic

instance ToJSON Message

api :: Proxy API
api = Proxy


-- if we use Text instead of Message then both handlers
-- will look the same and may be confused

echoMessage :: Text -> Handler Message
echoMessage msg = pure $ Message msg

sayHello :: Text -> Handler Text
sayHello name = pure $ "Hello " <> name <> "!"


server :: Server API
server = echoMessage
    :<|> sayHello

runApp = run 8080 (serve api server)
