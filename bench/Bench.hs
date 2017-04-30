module Bench where

-- Criterion
import Criterion.Main
    
main = defaultMain [
              bench "empty_bench" $ nfIO (return ())
         ]
