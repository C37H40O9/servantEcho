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
import Universum

type API = "echo" :> Capture "message" Text :> Get '[JSON] Message
      :<|> "sayHello"  :> QueryParam "name" Text :> Get '[JSON] Text
      :<|> AreaAPI

type AreaAPI = "area" :> 
  (    ReqBody '[JSON] Shape :> Post '[JSON] Double
  :<|> "shapes" :> Get '[JSON] [Shape]
  )

newtype Message = Message { msg :: Text }
  deriving Generic

instance ToJSON Message

api :: Proxy API
api = Proxy

data Shape = Circle Double | Square Double
    deriving Generic

instance ToJSON Shape
instance FromJSON Shape

shapeServer :: Server AreaAPI
shapeServer = area :<|> shapes

area :: Shape -> Handler Double
area (Circle r) = pure $ pi * r * r
area (Square s) = pure $ s * s

shapes :: Handler [Shape]
shapes = pure [Circle 5, Square 25]

echoMessage :: Text -> Handler Message
echoMessage msg = pure $ Message msg

sayHello :: Maybe Text -> Handler Text
sayHello name = pure $ case name of
  Just n -> "Hello " <> n <> "!"
  Nothing -> "Hello, stranger!"


server :: Server API
server = echoMessage
    :<|> sayHello
    :<|> shapeServer

runApp = run 8080 (serve api server)
