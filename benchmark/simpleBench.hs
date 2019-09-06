
import Gauge
import WebApp

action = pure ()

main :: IO ()
main = do
    defaultMain
        [ bench "Pure unit" $ nfIO $ action
        ]