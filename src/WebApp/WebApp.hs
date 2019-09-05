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
import Control.Monad.Free

import qualified Control.Logging as L
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- import Universum

type API = "echo" :> Capture "message" Text :> Get '[JSON] Message
      :<|> "sayHello"  :> QueryParam "name" Text :> Get '[JSON] Text
      :<|> AreaAPI

type AreaAPI = "area" :>
  (    ReqBody '[JSON] Shape :> Post '[JSON] Double
  :<|> "shapes" :> Get '[JSON] [Shape]
  )

newtype Message = Message { msg :: Text }
  deriving (Generic, Show, Eq)

instance ToJSON Message
instance FromJSON Message

data LoggerF next where
  LogMessage :: Text -> (() -> next) -> LoggerF next
  deriving (Functor)

type LoggerL = Free LoggerF

class Logger m where
  logMsg :: Text -> m ()

instance Logger LoggerL where
  logMsg msg = liftF $ LogMessage msg id

interpretLoggerF :: LoggerF a -> IO a
interpretLoggerF (LogMessage msg next) = do
    L.withStdoutLogging $ L.log msg
    pure $ next ()

runLoggerL :: LoggerL () -> IO ()
runLoggerL = foldFree interpretLoggerF

data Shape = Circle Double | Square Double
  deriving (Generic, Show, Eq)

instance ToJSON Shape
instance FromJSON Shape

shapeServer :: Server AreaAPI
shapeServer = serveArea :<|> shapes

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Square s) = s * s

serveArea :: Shape -> Handler Double
serveArea = pure.area

shapes :: Handler [Shape]
shapes = pure [Circle 5, Square 25]

data EchoF next
  = SayHello Text next
  | Echo Text next
  deriving (Functor)

type EchoL = Free EchoF

class Echo m where
    sayHello :: Maybe Text -> m Text
    echo :: Text -> m Text

instance Echo EchoL where
    sayHello (Just name) = liftF $ SayHello name name
    sayHello Nothing = liftF $ SayHello "stranger" "stranger"
    echo msg = liftF $ Echo msg msg

runEchoL :: EchoL Text -> Handler Text
runEchoL eff = case eff of
  Pure r -> pure r
  Free (SayHello name next) -> do
    let str = "Hello, " <> name <> "!"
    liftIO $ runLoggerL $ logMsg str
    pure str
  Free (Echo msg next) -> do
    liftIO $ runLoggerL $ logMsg msg
    pure msg

echo' :: Text -> Handler Message
echo' msg = Message <$> runEchoL (echo msg)

sayHello' :: Maybe Text -> Handler Text
sayHello' name = runEchoL $ sayHello name

api :: Proxy API
api = Proxy

server :: Server API
server = echo' :<|> sayHello' :<|> shapeServer

runApp = run 8080 (serve api server)
