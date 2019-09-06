module Main where

import System.Environment (lookupEnv)
import WebApp (runApp, BackendType(..), parseBackendType)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  env <- lookupEnv "BACKEND"
  let backendType = fromMaybe FreeMonad (env >>= parseBackendType)
  putStrLn $ "Running with " ++ show backendType ++ " as backend..."
  runApp backendType
