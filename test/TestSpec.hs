{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}
module TestSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import           Control.Exception                (bracket)
import           Control.Lens              hiding (Context)
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

testApp = serve api server

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
    let echo' :<|> sayHello' :<|> serveArea :<|> shapes = client api
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- testing scenarios start here
    describe "POST /area" $ do
      it "should calculate square area" $ \port -> do
        result <- runClientM (serveArea (Square 5)) (clientEnv port)
        result `shouldBe` (Right $ 25.0)

      it "will it fail ?" $ \port -> do
        result <- runClientM (serveArea (Square 5)) (clientEnv port)
        result `shouldBe` (Right $ 16.0)

      it "echo test" $ \port -> do
        result <- runClientM (echo' "Hello") (clientEnv port)
        result `shouldBe` (Right (Message "Hello"))

      it "sayHello with name test" $ \port -> do
        result <- runClientM (sayHello' (Just "Gendalf")) (clientEnv port)
        result `shouldBe` (Right "Hello, Gendalf!")

      it "sayHello without name test" $ \port -> do
        result <- runClientM (sayHello' Nothing) (clientEnv port)
        result `shouldBe` (Right "Hello, stranger!")

      it "shapes" $ \port -> do
        result <- runClientM (shapes) (clientEnv port)
        result `shouldBe` (Right [Circle 5.0,Square 25.0])