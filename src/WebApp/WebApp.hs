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
import Control.Monad.Freer.Internal hiding (run)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Natural

import qualified Control.Monad.Freer as F
import qualified Control.Monad.Free.Church as CE
import qualified Control.Logging as L
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Servant.Docs as SD

data BackendType = FreeMonad | ChurchEncodedFreeMonad | FreerMonad
  deriving (Eq, Show)

parseBackendType :: String -> Maybe BackendType
parseBackendType str = case str of
  "FM" -> Just FreeMonad
  "CEFM" -> Just ChurchEncodedFreeMonad
  "FR" -> Just FreerMonad
  _ -> Nothing

type EchoHandler = ReaderT BackendType (ExceptT ServerError IO)

echoHandlerToHandler :: BackendType -> EchoHandler :~> Handler
echoHandlerToHandler env = NT echoHandlerToHandler'
  where
  echoHandlerToHandler' :: EchoHandler a -> Handler a
  echoHandlerToHandler' h = Handler $ runReaderT h env

type API = "echo" :> Capture "message" Text :> Get '[JSON] Text

type APIWithDocs = ServantDocsAPI :<|> SwaggerAPI :<|> API

class Echo m where
    echo :: Text -> m Text

data EchoF next
  = Echo Text next
  deriving (Functor)

-- Free monad
type EchoL = Free EchoF

instance Echo EchoL where
    echo msg = liftF $ Echo msg msg

runEchoLFM :: EchoL Text -> Handler Text
runEchoLFM eff = case eff of
  Pure r -> pure r
  Free (Echo msg next) -> pure msg

-- Church-encoded free monad
type EchoLCE = CE.F EchoF

instance Echo EchoLCE where
  echo msg = CE.liftF $ Echo msg msg

matchF :: Functor f => (a -> r) -> (f r -> r) -> CE.F f a -> r
matchF kp kf f = CE.runF f kp kf

runEchoLCEFM :: EchoLCE Text -> Handler Text
runEchoLCEFM = matchF pure (\(Echo msg _) -> pure msg)

-- Freer
data EchoFreerL a where
  EchoFreer :: Text -> EchoFreerL Text

echoFreer :: F.Member EchoFreerL effs => Text -> F.Eff effs Text
echoFreer msg = F.send (EchoFreer msg)

instance Echo (Eff '[EchoFreerL]) where
  -- echo :: Text -> Eff '[EchoFreerL] Text
  echo = pure

runEchoFreer :: F.Eff '[EchoFreerL] Text -> Handler Text
runEchoFreer (Val x) = pure x
runEchoFreer (E u q) = case extract u of
  (EchoFreer msg) -> pure msg >>= \s -> runEchoFreer (qApp q s)

echoHandler :: Text -> EchoHandler Text
echoHandler msg = do
  b <- ask
  case b of
    FreeMonad -> lift $ runHandler' $ runEchoLFM (echo msg)
    ChurchEncodedFreeMonad -> lift $ runHandler' $ runEchoLCEFM (echo msg)
    FreerMonad -> lift $ runHandler' $ runEchoFreer (echo msg)

api :: Proxy API
api = Proxy

apiWithDocs :: Proxy APIWithDocs
apiWithDocs = Proxy

server :: BackendType -> Server APIWithDocs
server env = pure servantDocsAPI :<|> pure swaggerAPI :<|> hoistServer api (unwrapNT (echoHandlerToHandler env)) echoHandler

runApp env = run 8080 (serve apiWithDocs (server env))


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
