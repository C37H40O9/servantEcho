
import Gauge
import WebApp

action = pure ()

params = [1..1000]
circles= map Circle params
squares = map Square params


main :: IO ()
main = do
    defaultMain
        [ bench "Pure unit" $ nfIO $ action
        , bench "Squares area" $ nf  (area <$>) squares
        , bench "Circles area" $ nf  (area <$>) circles
        ]