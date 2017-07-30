-- |
-- Module      :  Bench
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
module Bench where

-- Qif
import Data.Qif

-- Data
import Data.Char(isPrint)
import qualified Data.Time.Calendar as Date

-- Criterion
import Criterion.Main

-- Test
import Test.QuickCheck

main :: IO ()
main = defaultMain [
              bench "generateSerialiseDeserialseBench" $ nfIO generateSerialiseDeserialseBench
         ]

generateSerialiseDeserialseBench :: IO ()
generateSerialiseDeserialseBench = 
    quickCheck . property $ forAll qif $ \_qif -> qifFromString (displayQif _qif) == _qif

dateGenerator :: Gen Date.Day 
dateGenerator = do _d <- choose (1, 31)
                   _m <- choose (1, 12)
                   _y <- choose (2012, 2018)
                   return $ Date.fromGregorian _y _m _d

transaction :: Gen Transaction
transaction = do
   j <- choose(0,20)
   k <- choose(0,20)
   c <- choose(0,20)
   iv  <- choose(0,5)
   c'  <- choose(False,True)
   iv' <- choose(False,True)
   description <- suchThat (vector j) isText
   text        <- suchThat (vector k) isText
   balance     <- choose(-5000.0,5000.0)
   _category   <- maybeGen c'  <$> suchThat (vector c) isText
   inv_act     <- maybeGen iv' <$> suchThat (vector iv) isText
   clr_sta     <- oneof $ map return [NotCleared, Cleared, Reconciled] 
   _date       <- dateGenerator
   return Transaction{ _date = _date, _payee = description, _memo = text, _amount = balance
                     , _category = _category, _investmentAction = inv_act, _clearedStatus = clr_sta, _splits = [] }
  where maybeGen False _  = Nothing
        maybeGen True  x  = Just x

qif :: Gen Qif
qif = do typeinfo  <- oneof $ map return [ Cash, Bank, CCard, Invst, OthA, OthL, Invoice ]
         _transactions <- listOf transaction
         return Qif{ accountType = typeinfo, transactions = _transactions }

isText :: String -> Bool
isText = all isPrint
