-- |
-- Module      :  Lib
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--   TODO: write description
module Lib where

{-# NOINLINE someFunc #-}
someFunc :: IO ()
someFunc = do putStrLn "someFunc"
              return ()

someOtherFunc x = do putStrLn $ show x ++ "in someOtherFunc"
                     return (x, show x)
