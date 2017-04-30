module Bench where

-- Criterion
import Criterion.Main
    
main = do defaultMain [
                 bench "empty_bench" $ nfIO (return ())
            ]