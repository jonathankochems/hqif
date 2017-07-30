-- |
-- Module      :  ParseSpec
-- Copyright   :  JOnathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ParseSpec where

import Prelude hiding (abs)
import Control.Lens

------------  below 
{-Imports for testing-}
import Test.Hspec
import Test.QuickCheck

{-Basic libraries-}
import Control.Applicative ((<$>))

import Data.Qif.Sample 

import Data.Maybe(isNothing)

import qualified Data.Qif as HQif
import Debug.Trace(trace)
import qualified Text.Parsec as Parsec


spec :: SpecWith ()
spec = 
  describe "Data" $ do
    it "type_parser - should parse partial type with a best guess" $ do
        let _type      = "CL"
            parsedType = Just "CLP" 
        either (\s -> error $ "fail:" ++ show s) 
                id 
                (Parsec.parse typeParser "(unknown)" _type) `shouldBe` parsedType
    it "parseDescription - should parse date and type" $ do
        let desc = "This is all a bla bla ON 27 AUG CLP"
            parsedTransaction =  Transaction{ transactionType = Just "CLP"
                                            , remainder = "This is all a bla bla"
                                            , date      = Just (27,"AUG")
                                            }
        either (\_ -> error "fail") 
                id 
                (parseDescription desc) `shouldBe` parsedTransaction
    it "parseDescription - should parse date and partial type with a best guess" $ do
        let desc = "This is all a bla bla ON 27 AUG CL"
            parsedTransaction =  Transaction{ transactionType = Just "CLP"
                                            , remainder = "This is all a bla bla"
                                            , date      = Just (27,"AUG")
                                            }
        either (\_ -> error "fail") 
                id 
                (parseDescription desc) `shouldBe` parsedTransaction
    it "parseDescription - should fallback to identity if there is no date or type" $ do
        let desc = "BOOTS STORES"
            parsedTransaction =  Transaction{ transactionType = Nothing
                                            , remainder = "BOOTS STORES"
                                            , date      = Nothing
                                            }
        either (\_ -> error "fail") 
                id 
                (parseDescription desc) `shouldBe` parsedTransaction
    it "parseDescription - should fallback to identity if there is no date or type" $ do
        let desc = "uZn7sBfcXJ35crVNGo1kPY ON 01 JUL BBP"
            parsedTransaction =  Transaction { transactionType = Just "BBP"
                                             , remainder = "uZn7sBfcXJ35crVNGo1kPY"
                                             , date = Just (1, "JUL")} 
    
        either (\_ -> error "fail") 
                id 
                (parseDescription desc) `shouldBe` parsedTransaction
    it "parseDescription - should parse type without date" $ do
        let desc :: String
            t    = Transaction {transactionType = Nothing, remainder = "TOWER HILL 1234567890", date = Nothing}
            desc = "TOWER HILL 1234567890 STO"
        either (\_ -> error "fail") 
                id 
                (parseDescription desc) `shouldBe` t    
    it "parseDescription - should swallow whitespace at the end" $ do
        let desc :: String
            t    = Transaction {transactionType = Nothing, remainder = "yQkzt54O", date = Nothing}
            t'   = Transaction {transactionType = Nothing, remainder = "yQkzt54O ", date = Nothing}
            desc = printTransaction t'
        either (\_ -> error "fail") 
                id 
                (parseDescription desc) `shouldBe` t    
    it "parseDescription - should swallow whitespace at the end #2" $ do
        let desc :: String
            t    = Transaction {transactionType = Nothing, remainder = " ZKLqnrpE2RFF r5nK6M4", date = Nothing}
            t'   = Transaction {transactionType = Nothing, remainder = " ZKLqnrpE2RFF r5nK6M4 U", date = Nothing}
            desc = printTransaction t'
        either (\_ -> error "fail") 
                id 
                (parseDescription desc) `shouldBe` t    
    it "parseDescription - should not parse transactionType when there isn't any" $ do
        let desc = printTransaction parsedTransaction
            parsedTransaction =  Transaction { transactionType = Nothing
                                             , remainder = "jotO36Df9WRnU 2E67ygnjofez N"
                                             , date = Just (17,"JAN")} 
        either (\_ -> error "fail") 
                id 
                (parseDescription desc) `shouldBe` parsedTransaction
    it "printTransaction - should work with no type and date" $ do
        let desc :: String
            desc = "BOOTS STORES"
            t =  Transaction{ transactionType = Nothing
                            , remainder = "BOOTS STORES"
                            , date      = Nothing
                            }
        desc `shouldBe` printTransaction t
    it "printTransaction - should work with with type and date" $ do
        let desc :: String
            desc = "This is all a bla bla ON 27 AUG CLP"
            t = Transaction{ transactionType = Just "CLP"
                           , remainder = "This is all a bla bla"
                           , date      = Just (27,"AUG")
                           }
        desc `shouldBe` printTransaction t
    it "composing parsing and pretty printing should yield the identity" $
      do let _swallowBackWhiteSpace = reverse . dropWhile (== ' '). reverse
             swallowBackWhiteSpace t 
               | isNothing (date t) && isNothing (transactionType t) = t{ remainder = _swallowBackWhiteSpace $ remainder t }
               | otherwise = t
         property $ forAll (transactionGenerator =<< merchantCandidatesGenerator) $ 
           \t -> either (const t) id (parseDescription $ printTransaction t) `equals` swallowBackWhiteSpace t
    it "sampling qif transactions respects chosen categories, balance range, and dates are consistent" $
      property $ forAll (do 
                    ms <- map (\s -> (s, Just "cat")) <$> merchantCandidatesGenerator
                    hqifTransactionGenerator ms (-100.50,1234.56)
                    ) $ 
           \t -> do t ^. HQif.category `shouldBe` Just "cat"
                    let b = t ^. HQif.amount
                    ((-100.50) - 10.05) <= b && b <= (1234.56 + 123.456) `shouldBe` True
                    -- TODO(JAK): check more cat consistency)
                    --            check date consistency
                    
infixl 9 `equals`
equals :: (Show a, Eq a) => a -> a -> Bool
equals x y | x == y    = True
           | otherwise = flip trace False $
                           "   expected x and y to be equal, where \n"
                        ++ "     x: " ++ show x ++ "\n"
                        ++ "     y: " ++ show y ++ "\n"
                         
                                   


