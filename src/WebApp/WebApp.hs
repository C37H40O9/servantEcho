module WebApp
  where


import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Servant
import Servant.API
import Servant.Client

import Servant.Docs (ToSample, ToCapture, DocIntro(..), DocCapture(..), markdown, singleSample, docs)
import Servant.Server
import Servant.Swagger
import Network.Wai.Handler.Warp
import Control.Monad.Free

import qualified Control.Logging as L
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Servant.Docs as SD


type API = "echo" :> Capture "message" Text :> Get '[JSON] Text

type APIWithDocs = ServantDocsAPI :<|> SwaggerAPI :<|> API

data EchoF next
  = SayHello Text next
  | Echo Text next
  deriving (Functor)

type EchoL = Free EchoF

class Echo m where
    sayHello :: Maybe Text -> m Text
    echo :: Text -> m Text

instance Echo EchoL where
 --   sayHello (Just name) = liftF $ SayHello name name
 --   sayHello Nothing = liftF $ SayHello "stranger" "stranger"
    echo msg = liftF $ Echo msg msg

runEchoL :: EchoL Text -> Handler Text
runEchoL eff = case eff of
  Pure r -> pure r
 -- Free (SayHello name next) -> do
 --   let str = "Hello, " <> name <> "!"
 --   liftIO $ runLoggerL $ logMsg str
 --   pure str
  Free (Echo msg next) -> do
   -- liftIO $ runLoggerL $ logMsg msg
    pure msg

echo' :: Text -> Handler Text
echo' msg = runEchoL (echo msg)

api :: Proxy API
api = Proxy

apiWithDocs :: Proxy APIWithDocs
apiWithDocs = Proxy

server :: Server APIWithDocs
server = (pure servantDocsAPI) :<|> (pure swaggerAPI) :<|> echo'

runApp = run 8080 (serve apiWithDocs server)


-- Swagger part

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Swagger spec for API.
swaggerAPI :: Swagger
swaggerAPI = toSwagger api
  & info.title   .~ "Example API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"

-- Servant-docs part

type ServantDocsAPI = "servant-docs.md" :> Get '[PlainText] String

servantDocsAPI :: String
servantDocsAPI = echoDocs

instance ToCapture (Capture "message" Text) where
  toCapture _ = DocCapture "message" "message to echo"

instance ToSample Text where
  toSamples _ = singleSample "Text sample"



echoDocs = markdown (docs api :: SD.API)