-- |
-- Module      :  HQifSpec
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module HQifSpec where

import Data.Qif

-- Data
import Data.Char(isPrint)
import qualified Data.Time.Calendar as Date

-- Test
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen(oneof)

spec = do
  describe "Data.Qif" $ do
    it "should satisfy that displaying and parsing yields the identity" $ do
      let qif = Qif {accountType = Bank, transactions = [Transaction {_date = Date.fromGregorian 2018 4 3, _payee = "r\227\245\&5\191!", _memo = ";\226S\224:+>aC1ik", _amount = -1037.0022, _category = Just "D:RlR", _investmentAction = Nothing, _clearedStatus = NotCleared, _splits = []}]}
          _parsedQif = qifFromString $ displayQif qif
      (displayQif _parsedQif) `shouldBe` (displayQif qif)
      _parsedQif `shouldBe` qif
    it "should satisfy that displaying and parsing yields the identity" $ do
      let qif = Qif { accountType = Invoice
                    , transactions = [Transaction {_date = Date.fromGregorian 2013 9 24, _payee = "\253y\247K\201\201", _memo = "\225+pA\239\&1k[", _amount = -3760.5286, _category = Just "\183j\253O`_yZ\245Xz+8\231e", _investmentAction = Nothing, _clearedStatus = Cleared, _splits = []}
                                     ,Transaction {_date = Date.fromGregorian 2014 8 17, _payee = "\198\\trrw}", _memo = "4\223},Z\243\&1]o=", _amount = -262.1128, _category = Nothing, _investmentAction = Just "g|6", _clearedStatus = Reconciled, _splits = []}
                                     ,Transaction {_date = Date.fromGregorian 2016 6 21, _payee = "W8", _memo = "'O15$\172W\234.KC>h\172\207|S", _amount = 2330.6196, _category = Just "xM1X\228\206", _investmentAction = Nothing, _clearedStatus = Reconciled, _splits = []}
                                     ,Transaction {_date = Date.fromGregorian 2017 4 30, _payee = "_dRdm\213C\"\175dnT@ Kz\210", _memo = "p\197AN%\"tP\233)eh9o^\233;", _amount = -3659.8916, _category = Nothing, _investmentAction = Nothing, _clearedStatus = NotCleared, _splits = []}
                                     ]
                    }
          _parsedQif = qifFromString $ displayQif qif
      (displayQif _parsedQif) `shouldBe` (displayQif qif)
      _parsedQif `shouldBe` qif
    it "should be able to take a qif, print it as a string, and parse it and result in the original qif" $
      do property $ forAll qif $ \qif -> (qifFromString $ displayQif qif) == qif


date_generator :: Gen Date.Day 
date_generator = do _d <- choose (1, 31)
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
   s'  <- choose(False,True)
   description <- suchThat (vector j) isText
   text        <- suchThat (vector k) isText
   balance     <- choose(-5000.0,5000.0)
   category    <- maybeGen c'  <$> suchThat (vector c) isText
   inv_act     <- maybeGen iv' <$> suchThat (vector iv) isText
   clr_sta     <- oneof $ map return [NotCleared, Cleared, Reconciled] 
   date        <- date_generator
   return Transaction{ _date = date, _payee = description, _memo = text, _amount = balance
                     , _category = category, _investmentAction = inv_act, _clearedStatus = clr_sta
                     , _splits = [] }
  where maybeGen False _  = Nothing
        maybeGen True  x  = Just x

qif :: Gen Qif
qif = do typeinfo  <- oneof $ map return [ Cash, Bank, CCard, Invst, OthA, OthL, Invoice ]
         transactions <- listOf transaction
         return Qif{ accountType = typeinfo, transactions = transactions }

isText = all isPrint
