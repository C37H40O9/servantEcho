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

newtype Message = Message { msg :: Text }
  deriving Generic

instance ToJSON Message

api :: Proxy API
api = Proxy


echoMessage :: Text -> Handler Message
echoMessage msg = pure $ Message msg

sayHello :: Maybe Text -> Handler Text
sayHello name = pure $ case name of
  Just n -> "Hello " <> n <> "!"
  Nothing -> "Hello, stranger!"


server :: Server API
server = echoMessage
    :<|> sayHello

runApp = run 8080 (serve api server)
