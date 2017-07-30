-- |
-- Module      :  Data.Qif.Sample
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Qif.Sample where

-- Control
import Control.Monad(forM)
import Control.Arrow(first, second)

-- Data
import qualified Data.Qif as HQif
import Data.Maybe(isJust, fromMaybe)
import Data.List(elemIndex, sort)
import Data.Time.Calendar(fromGregorian)

-- Test
import Test.QuickCheck


data Transaction = Transaction{
     transactionType :: Maybe String
   , remainder :: String  
   , date :: Maybe (Int,String)  
       } deriving (Show,Eq)

transactionTypes :: [String]
transactionTypes = sort [ apr, atm, bbp, bcc, bgc  
                         , cat  , chq  , cdl  , clp, chaps
                         , cpm  , cre, ddr  , dr   , ft, tfr 
                         , iban , imo  , isa, _rem  
                         , rev  , sto  , unp  ]
  where
    apr   = "APR"    -- Annual Percentage Rate (relates to credit interest)
    atm   = "ATM"    -- Cash Withdrawal
    bbp   = "BBP"    -- bill payment
    bcc   = "BCC"    -- Barclays Connect Card
    bgc   = "BGC"    -- bank giro credit
    cat   = "CAT"    -- a standard applied to ISAs that stands for reasonable Charges, easy Access, fair Terms
    chq   = "CHQ"    -- Cheque
    cdl   = "CDL"    -- Career Development Loan
    clp   = "CLP"    -- Contactless Payment
    chaps = "CHAPS"  -- Clearing House Automated Payment System (a means of transferring money)
    cre   = "CRE"    -- Credit payment
    cpm   = "CPM"    -- Contactless Payment??
    ddr   = "DDR"    -- Direct Debit
    dr    = "DR"     -- debit balance (overdrawn)
    ft    = "FT"     -- Transfer
    tfr   = "TFR"    -- Transfer
    iban  = "IBAN"   -- International Bank Account Number (you can find this on your statement)
    imo   = "IMO"    -- International Money Order
    isa   = "ISA"    -- Individual Savings Account
    _rem   = "REM"    -- remittance: a cheque credited to your account that was not paid in at your account-holding branch or bank
    rev   = "REV"    -- reversal: a standing order or Direct Debit has been recalled
    sto   = "STO"    -- standing order
    unp   = "UNP"    -- unpaid


months :: [String]
months = [ "JAN", "FEB", "MAR", "APR", "MAY"
         , "JUN", "JUL", "AUG", "SEP", "OCT"
         , "NOV", "DEC" ]

printTransaction :: Transaction -> String
printTransaction t = remainder t ++ d ++ t'
  where d = fromMaybe ""$ do
              (_d,_m) <- date t
              let _d' | _d >= 10  = show _d
                      | otherwise = '0':show _d
              return $ " ON " ++ _d' ++ " " ++ _m
        t' = fromMaybe "" $ do
               _t <- transactionType t
               return $ ' ' : _t



-- TODO(JAK): Move this into package hqif
chooseFromList :: [a] -> Gen a
chooseFromList xs = do i <- choose(0, length xs - 1)  
                       return $ xs !! i

transactionTypeGenerator :: Gen (Maybe String)
transactionTypeGenerator = chooseFromList tts
  where tts = Nothing:map Just transactionTypes

dateGenerator :: Gen (Maybe (Int,String))
dateGenerator = do m <- chooseFromList ms
                   d <- choose(1, 31)  
                   return $ do _m <- m 
                               return (d, _m) 
  where ms = Nothing:map Just months

merchantGenerator :: [String] -> Gen String
merchantGenerator [] = return ""
merchantGenerator ms = chooseFromList ms


merchantCandidatesGenerator :: Gen [String]
merchantCandidatesGenerator = do (k :: Int) <- choose(1,10)
                                 forM [0..k] $ \_ -> do
                                   (l::Int) <- choose(1,46)
                                   forM [1..l] $ \_ -> chooseFromList (' ':['a'..'z'] ++ ' ':['A'..'Z'] ++ ' ':['0'..'9'] :: String)

transactionGenerator :: [String] ->  Gen Transaction
transactionGenerator mcs = do _type <- transactionTypeGenerator
                              _date <- dateGenerator
                              m     <- merchantGenerator mcs
                              let _type' | isJust _date = _type
                                         | otherwise    = Nothing
                              return Transaction{ transactionType = _type' 
                                                , remainder       = m
                                                , date            = _date 
                                                }

sampleTransactions :: [(String, Maybe String)] -> (Float, Float) -> IO [HQif.Transaction]
sampleTransactions mcs brange = sample' $ hqifTransactionGenerator mcs brange

hqifTransactionGenerator :: [(String, Maybe String)] -> (Float, Float) -> Gen HQif.Transaction
hqifTransactionGenerator []  _             = error "empty mcs"
hqifTransactionGenerator mcs balance_range = do 
    (m,c) <- chooseFromList mcs
    t <- transactionGenerator [m]
    _minZ <- choose (-1.1, -1.0)
    _maxZ <- choose (1.0, 1.1)
    _balance <- choose . second (_maxZ *) $ first (_minZ *) balance_range
    _d' <- choose (1, 31)
    _m' <- choose (1, 12)
    _y' <- choose (2012, 2018)
    let _category          = c
        _text              = ""
        _investment_action = Nothing
        _cleared_status    = HQif.Cleared
        _description       = printTransaction t
        (_d,_m,_y)         = fromMaybe (_d',_m',_y') $ do 
                               (p_d,p_m) <- date t
                               __m <- (1 +) <$> p_m `elemIndex` months
                               return (p_d, __m, _y')
        _date              = fromGregorian _y _m _d
    return HQif.Transaction {
         HQif._date             = _date
       , HQif._payee            = _description
       , HQif._memo             = _text
       , HQif._amount           = _balance
       , HQif._category         = _category
       , HQif._investmentAction = _investment_action
       , HQif._clearedStatus    = _cleared_status 
       , HQif._splits           = []
    }
