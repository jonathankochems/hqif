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
module LibTwo where

import Lib

someFuncThree = someFunc 

someOtherFuncFour :: Show a => a -> IO (a, String)
someOtherFuncFour = someOtherFunc 
