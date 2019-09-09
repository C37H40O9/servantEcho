
import Control.Concurrent
import Gauge
import System.Process (shell, readCreateProcess)
import WebApp

runCmd cmd = readCreateProcess (shell cmd) ""

runWRK route = runCmd $ "wrk -t2 -c5 -d5s -H 'Host: example.com' --timeout 2s http://localhost:8080" ++ route

action = pure ()

main :: IO ()
main = do
    -- running the application and workload generator on the same machine is not a good idea
    tId <- forkIO $ runApp FreeMonad
    putStrLn "Installed wrk required"
    putStrLn =<< runWRK "/echo/hi"
    putStrLn =<< runWRK "/swagger.json"
    putStrLn =<< runWRK "/servant-docs.md"
    killThread tId
    defaultMain
        [ bench "Pure unit" $ nfIO $ action
        ]