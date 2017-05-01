-- |
-- Module      :  Data.Qif.Types
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--  https://en.wikipedia.org/wiki/Quicken_Interchange_Format 
--
-----------------------------------------------------------------------------


module Data.Qif.Types where

-- Data
import Data.Maybe (fromMaybe)
import qualified Data.Time.Calendar as Date
import Data.Time.Format(formatTime,defaultTimeLocale)
import Data.Functor.Identity    

-- Control
import Control.Applicative

data Qif = Qif{ accountType  :: AccountType 
              , transactions :: [Transaction]}
    deriving (Eq, Show)

data AccountType =
   Cash    -- ^ !Type:Cash    Cash Flow: Cash Account
 | Bank    -- ^ !Type:Bank    Cash Flow: Checking Account
 | CCard   -- ^ !Type:CCard   Cash Flow: Credit Card Account
 | Invst   -- ^ !Type:Invst   Investing: Investment Account
 | OthA    -- ^ !Type:Oth A   Property & Debt: Asset
 | OthL    -- ^ !Type:Oth L   Property & Debt: Liability
 | Invoice -- ^ !Type:Invoice Invoice (Quicken for Business only) 
  deriving (Show, Read, Eq) 

data ClearedStatus =
   NotCleared -- ^ not cleared: blank 
 | Cleared    -- ^ cleared: "*" or "c"
 | Reconciled -- ^ reconciled: "X" or "R"
  deriving (Show, Read, Eq, Ord) 

data Transaction = Transaction {  _date             :: Date.Day       -- ^ D Date. Leading zeroes on month and day can be skipped. Year can be either 4 digits or 2 digits or '6 (=2006). Example:	D25 December 2006
                                , _payee            :: String         -- ^ P Payee. Or a description for deposits, transfers, etc. Example: PStandard Oil, Inc.
                                , _memo             :: String         -- ^ M Memo—any text you want to record about the item. Example: Mgasoline for my car
                                , _amount           :: Float          -- ^ T Amount of the item. For payments, a leading minus sign is required. For deposits, either no sign or a leading plus sign is accepted. Do not include currency symbols ($, £, ¥, etc.). Comma separators between thousands are allowed. Example: T-1,234.50
                                , _category         :: Maybe String   -- ^ L Category or Transfer and (optionally) Class. The literal values are those defined in the Quicken Category list. SubCategories can be indicated by a colon (":") followed by the subcategory literal. If the Quicken file uses Classes, this can be indicated by a slash ("/") followed by the class literal. For Investments, MiscIncX or MiscExpX actions, Category/class or transfer/class. (40 characters maximum) Example: LFuel:car
                                , _investmentAction :: Maybe String   -- ^ N Investment Action (Buy, Sell, etc.). Example: NBuy
                                , _clearedStatus    :: ClearedStatus  -- ^ C Cleared status. Values are blank (not cleared), "*" or "c" (cleared) and "X" or "R" (reconciled). Example: CR
                                , _splits           :: [(Maybe String 
                                                       , String       
                                                       , Float )]     -- ^ S Split category. Same format as L (Categorization) field. (40 characters maximum)	Sgas from Esso 
                                                                      --   E Split memo—any text to go with this split item. Ework trips
                                                                      --   $ Amount for this split of the item. Same format as T field. $1,000.50
                               } deriving (Eq, Show, Ord, Read)       
-- lenses:
-- date :: Lens' (Transaction) Date.Day
date :: Functor f => (Date.Day -> f Date.Day) -> Transaction -> f Transaction
date f t = fmap (\d' -> t{_date = d'}) (f $ _date t)

-- payee :: Lens' (Transaction) String
payee :: Functor f => (String -> f String) -> Transaction -> f Transaction
payee f t = fmap (\d' -> t{_payee = d'}) (f $ _payee t)

-- memo :: Lens' (Transaction) String
memo :: Functor f => (String -> f String) -> Transaction -> f Transaction
memo f t = fmap (\d' -> t{_memo = d'}) (f $ _memo t)

-- amount :: Lens' (Transaction) Float
amount :: Functor f => (Float -> f Float) -> Transaction -> f Transaction
amount f t = fmap (\d' -> t{_amount = d'}) (f $ _amount t)

-- category :: Lens' (Transaction) (Maybe String)
category :: Functor f => (Maybe String -> f (Maybe String)) -> Transaction -> f Transaction
category f t = fmap (\d' -> t{_category = d'}) (f $ _category t)

-- investmentAction :: Lens' (Transaction) (Maybe String)
investmentAction :: Functor f => (Maybe String -> f (Maybe String)) -> Transaction -> f Transaction
investmentAction f t = fmap (\d' -> t{_investmentAction = d'}) (f $ _investmentAction t)

-- clearedStatus :: Lens' (Transaction) ClearedStatus
clearedStatus :: Functor f => (ClearedStatus -> f ClearedStatus) -> Transaction -> f Transaction
clearedStatus f t = fmap (\d' -> t{_clearedStatus = d'}) (f $ _clearedStatus t)

-- splits :: Lens' (Transaction) [Maybe String, String, Float]
splits :: Functor f => ([(Maybe String, String, Float)] -> f [(Maybe String, String, Float)]) -> Transaction -> f Transaction
splits f t = fmap (\d' -> t{_splits = d'}) (f $ _splits t)
