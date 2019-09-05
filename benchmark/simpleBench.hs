
import Gauge
import WebApp

action = pure ()

main :: IO ()
main = do
    defaultMain [bench "name" $ nfIO $ action]