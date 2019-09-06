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
import Control.Concurrent.STM
import Control.Monad.Trans.Free (FreeT, iterT)
import Control.Monad.Reader
import Data.Set
import Control.Natural
import Control.Monad.Free.Church (improve)

import qualified Control.Logging as L
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- import Universum

data FMType = FreeMonad | ChurchEncodedFreeMonad
data BackendType = SomeFreeMonad FMType | FreerMonad

fmDecider :: FMType -> (forall f. Functor f => (forall m. MonadFree f m => m a) -> Free f a)
fmDecider FreeMonad = id
fmDecider ChurchEncodedFreeMonad = improve

-- | Env value (lifted to the type-level)
data Env = Prod | Dev | Test

-- TODO 1: make it work
-- -- | Environment-specific app type
-- type family StateDecider env where
--   StateDecider Prod = ()
--   StateDecider Dev = STM (Set User)
--   StateDecider Test = ()
--
-- -- | Environment-specific container type
-- type family ContainerEnv env a where
--   ContainerEnv Prod a = ()
--   ContainerEnv Dev a = [a]
--   ContainerEnv Test a = ()

-- | Base monad
type BaseM env = ReaderT (AppState env) IO
-- | Coproduct with other handlers
type AppHandlerF env = UserF
-- | Handler-sum free monad
type AppHandler env = Free (AppHandlerF env)
-- | Handler-sum free monad + base monad
type AppHandlerM env = FreeT (AppHandlerF env) (BaseM env)

-- | Application state
newtype AppState (env :: Env) = AppState { users :: STM (Set User) } -- StateDecider env User -- Ref 1

-- | If an environment has a handler it should provide user with some initial state and with a method to graft handler action into base monad
class HasHandler (env :: Env) where
  initialAppState :: (MonadIO m) => m (AppState env)
  handleAll :: AppHandlerM env a -> BaseM env a

class HasInterpreter (env :: Env) where
  interpretAll :: forall a. Proxy env -> AppEff env a -> AppHandler env a

instance HasHandler Dev where
  initialAppState = pure . AppState . pure $
    fromList [User { firstName = "Bruce", lastName = "Dowson"}, User { firstName = "Coney", lastName = "Royce" }]
  handleAll = iterT handleAll'
    where
      handleAll' :: AppHandlerF Dev (BaseM Dev a) -> BaseM Dev a
      handleAll' eff = join $ handleUser eff -- Handle coproduct

instance HasInterpreter Dev where
  interpretAll _ = foldFree interpretAll'
    where
      f :: forall a. AppF Dev a -> UserF a
      f (CreateUser usr next) = AddUser usr next
      f (ListUsers p f) = FindUsers p f
      interpretAll' :: forall a. AppF Dev a -> AppHandler Dev a
      interpretAll' (CreateUser user next) = do
        hoistFree f $ createUser user
        pure next
      interpretAll' (ListUsers p next) = next <$> hoistFree f (listUsers p)

data AppF (env :: Env) next
  = CreateUser User next
  | ListUsers (User -> Bool) ([User] -> next)
  deriving (Functor)
  -- ^ and some extra constructors (depends on effects)

type AppEff (env :: Env) = Free (AppF env)

createUser :: User -> AppEff eff ()
createUser user = liftF $ CreateUser user ()

listUsers :: (User -> Bool) -> AppEff env [User]
listUsers p = liftF $ ListUsers p id

-- | APi depends on environment and can pass it to sub-api's
type API env = "echo" :> Capture "message" Text :> Get '[JSON] Message
      :<|> "sayHello"  :> QueryParam "name" Text :> Get '[JSON] Text
      :<|> AreaAPI
      :<|> UsersAPI env

type AreaAPI = "area" :>
  (    ReqBody '[JSON] Shape :> Post '[JSON] Double
  :<|> "shapes" :> Get '[JSON] [Shape]
  )

-- TODO: way to enforce `ContainerEnv env a` be json'able for json'able a
type UsersAPI env = "users" :> Get '[JSON] [User] -- (ContainerEnv env User) -- Ref 1
                :<|> "users" :> "add"
                             :> Capture "firstName" Text :> Capture "lastName" Text
                             :> Get '[JSON] ()

api :: forall (env :: Env). Proxy env -> Proxy (API env)
api e = Proxy

-- | Domain types
data User = User { firstName :: Text, lastName :: Text }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON User
instance FromJSON User

newtype Message = Message { msg :: Text }
  deriving (Generic, Show, Eq)

instance ToJSON Message
instance FromJSON Message

data Shape = Circle Double | Square Double
  deriving (Generic, Show, Eq)

instance ToJSON Shape
instance FromJSON Shape

-- | And domain functions
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Square s) = s * s

--

-- | Some languages and their interpreters
data UserF next
  = AddUser User next
  | FindUsers (User -> Bool) ([User] -> next)
  deriving (Functor)

type UserL = Free UserF

class UserM m where
  addUser :: User -> m ()
  findUsers :: (User -> Bool) -> m [User]

instance UserM UserL where
  addUser user = liftF $ AddUser user ()
  findUsers p = liftF $ FindUsers p id

handleUser :: UserF a -> BaseM env a
handleUser (AddUser user next) = do
  (AppState st) <- ask
  liftIO $ atomically (insert user <$> st)
  pure next
handleUser (FindUsers p next) = do
  (AppState st) <- ask
  ls <- liftIO $ atomically (elems . Data.Set.filter p <$> st)
  pure (next ls)

handleDev :: AppHandlerM Dev a -> BaseM Dev a
handleDev = iterT handleAll'
  where
    handleAll' :: AppHandlerF Dev (BaseM Dev a) -> BaseM Dev a
    handleAll' eff = join $ handleUser eff

executeUnsafe :: forall (env :: Env) a. (HasHandler env, HasInterpreter env) => Proxy env -> AppState env -> AppEff env a -> IO a
executeUnsafe e initialState ops = runReaderT program initialState
  where
    program = handleAll $ toFreeT $ interpretAll e ops

nat :: (HasHandler env, HasInterpreter env) => Proxy env -> AppState env -> AppEff env :~> Servant.Handler
nat e state = NT $ \action -> liftIO $ executeUnsafe e state action

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

shapeServer :: Server AreaAPI
shapeServer = serveArea :<|> shapes

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

usersServer' :: (HasHandler env, HasInterpreter env) => Proxy env -> ServerT (UsersAPI env) (AppEff env)
usersServer' e = listUsers (const True) :<|> (\f l -> pure ()) -- TODO: 2 fix deduce problem (\f l -> (addUser :: User -> AppEff env ()) (User f l) >> pure ())

usersServer :: (HasHandler env, HasInterpreter env) => Proxy env -> AppState env -> Server (UsersAPI env)
usersServer e state = hoistServer (Proxy :: Proxy (UsersAPI env)) (unwrapNT $ nat e state) (usersServer' e)

server :: (HasHandler env, HasInterpreter env) => Proxy env -> AppState env -> Server (API env)
server e state = echo' :<|> sayHello' :<|> shapeServer :<|> usersServer e state

runApp :: IO ()
runApp = do
  st <- initialAppState :: IO (AppState Dev)
  run 8080 (serve (api (Proxy :: Proxy Dev)) (server (Proxy :: Proxy Dev) st))
