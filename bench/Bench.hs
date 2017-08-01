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
{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Bench where

-- Control
import Control.Monad(forM)

-- Text
import qualified Text.Parsec as Parsec

-- Qif
import Data.Qif
import Data.Qif.Sample(partialString, months, typeParser, transactionTypes)

-- Data
import Data.Char(isPrint)
import qualified Data.Time.Calendar as Date
import Data.Maybe(fromMaybe)
import Data.Functor.Identity(Identity)

-- Criterion
import Criterion.Main

-- Test
import Test.QuickCheck

main :: IO ()
main = defaultMain [
                 bench "generateSerialiseDeserialseBench" $ nfIO generateSerialiseDeserialseBench
               , bench "partial_string" $ nfIO (partialStringBench n)
               , bench "string"         $ nfIO (stringBench n)
               , bench "partial_types"  $ nfIO (partialTypesBench n)
               , bench "base"           $ nfIO (baseBench n)
            ]
  where n     =  1000

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

assert :: Monad m => Bool -> m ()
assert True  = return ()
assert False = error "assertion failure"

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse p = Parsec.parse p "(unknown)"

partialStringBench :: Int -> IO ()
partialStringBench n = do rs <- forM (zip [1..n] $ cycle months) $ \(_,m) -> do
                                  (!_rs) <- forM months $ \m' -> do 
                                              let result = either (const False) ((==) m') $ parse (partialString m) m' 
                                              return result
                                  return $ or _rs
                          assert $ and rs
                          return ()

partialTypesBench :: Int -> IO ()
partialTypesBench n = do rs <- forM (zip [1..n] $ cycle transactionTypes) $ \(_,t) -> do
                                 let result = either (const False) ((==) t) $ fromMaybe "" <$> parse (typeParser) t 
                                 return result
                         assert $ and rs
                         return ()

stringBench :: Int -> IO ()
stringBench n = do rs <- forM (zip [1..n] $ cycle months) $ \(_,m) -> do
                           (!_rs) <- forM months $ \m' -> do 
                                       let result = either (const False) ((==) m') $ parse (Parsec.string m) m' 
                                       return result
                           return $ or _rs
                   assert $ and rs
                   return ()

baseBench :: Int -> IO ()
baseBench n = do rs <- forM (zip [1..n] $ cycle months) $ \(_,m) -> do
                         (!_rs) <- forM months $ \m' -> do 
                                     let result = either (const False) ((==) m') $ const (Right m) m' 
                                     return result
                         return $ or _rs
                 assert $ and rs
                 return ()
