module TestSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import           Control.Exception                (bracket)
import           Control.Lens              hiding (Context)
import           Control.Natural
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict              as HM
import           Data.Text                        (Text, unpack)
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client
import           Servant.Server

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher


import WebApp

testApp = serve api (hoistServer api (unwrapNT (echoHandlerToHandler FreeMonad)) echoHandler)
  --serve api echo

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure testApp) action

spec :: Spec
spec =
  -- `around` will start our Server before the tests and turn it off after
  around withUserApp $ do
    -- create a test client function    
    let echo'  = client api
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- testing scenarios start here
    describe "API tests" $ do

      it "echo test" $ \port -> do
        let mesage = "Hello!"
        result <- runClientM (echo' mesage) (clientEnv port)
        result `shouldBe` (Right mesage)