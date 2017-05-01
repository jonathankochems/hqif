-- |
-- Module      :  Data.Qif.Internal
-- Copyright   :  Jonathan Kochems 2017
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
{-# LANGUAGE FlexibleContexts #-}
module Data.Qif.Internal where

import Data.Qif.Types

-- Text
import Text.ParserCombinators.Parsec

-- Data
import qualified Data.Time.Calendar as Date
import Data.Time.Format(formatTime,defaultTimeLocale,parseTimeM)
import Data.Maybe (fromMaybe)
import Data.List(intercalate)
import Data.Functor.Identity    

-- Control
import Control.Applicative

{-*********************************************
 * Display functions
 *********************************************-}
displayQif :: Qif -> String
displayQif = unlines . go
  where go :: Qif -> [String]
        go qif = (displayAccountType $ accountType qif) 
                  : (foldl (\ss s-> ss ++ s++["^"]) [] . map displayTransaction $ transactions qif)

displayTransaction :: Transaction -> [String]
displayTransaction trans = ["P" ++ p, "T" ++ a, "D" ++ d, "M" ++ _memo, "C" ++ clr] 
                ++ maybe [] (\a -> ["N" ++ a]) act 
                ++ maybe [] (\c -> ["L" ++ c]) cat
    where   p     = trans ^. payee
            a     = show $ trans ^. amount
            d     = displayDate $ trans ^. date
            _memo = trans ^. memo
            act   = trans ^. investmentAction
            clr   = displayClearedStatus $ trans ^. clearedStatus 
            cat   = trans ^. category

displayAccountType :: AccountType -> String
displayAccountType _type = "!Type:" ++ enum
  where enum = case _type of
                  Cash    -> "Cash"   
                  Bank    -> "Bank"   
                  CCard   -> "CCard"  
                  Invst   -> "Invst"  
                  OthA    -> "Oth A"  
                  OthL    -> "Oth L"  
                  Invoice -> "Invoice"

displayClearedStatus NotCleared = ""
displayClearedStatus Cleared    = "c"
displayClearedStatus Reconciled = "R"

displayDate d = formatTime defaultTimeLocale "%d/%m/%Y" d

{-*********************************************
 * Lens helpers
 *********************************************-}
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}
l .~ b = runIdentity . l (\_ -> Identity b)
{-# INLINE (.~) #-}
infixr 8 &
x & f = f x
{-# INLINE (&) #-}

{-*********************************************
 * helpers
 *********************************************-}
map_on_balance f x = x & amount .~ f (x ^. amount)

{- **************************************************
    Parsers for transaction fields
*************************************************** -}
-- | Helper type to tag transaction field data
data TransactionField =   D String | P String | M String 
                        | T String | N String | L String
                        | C String | S [(String, String, String)]


-- | Parser for date field in transaction
date_parser :: GenParser Char st TransactionField
date_parser = do _ <- string "D"
                 date <- manyTill (noneOf ['\n','\r']) (newline_or_eof)
                 return $ D date

-- | Parser for description field in transaction
description_parser :: GenParser Char st TransactionField
description_parser = do _ <- string "P"
                        description <- manyTill (noneOf ['\n','\r']) newline_or_eof
                        return $ P description

-- | Parser for text field in transaction
text_parser :: GenParser Char st TransactionField
text_parser = do _ <- string "M"
                 text <- manyTill (noneOf ['\n','\r']) newline_or_eof
                 return $ M text

-- | Parser for balance field in transaction
balance_parser :: GenParser Char st TransactionField
balance_parser = do _ <- string "T"
                    balance <- manyTill (noneOf ['\n','\r']) newline_or_eof
                    return $ T balance

-- | Parser for investment action field in transaction
investmentaction_parser :: GenParser Char st TransactionField
investmentaction_parser = do _ <- string "N"
                             action <- manyTill (noneOf ['\n','\r']) newline_or_eof
                             return $ N action

-- | Parser for investment action field in transaction
category_parser :: GenParser Char st TransactionField
category_parser = do _ <- string "L"
                     category <- manyTill (noneOf ['\n','\r']) newline_or_eof
                     return $ L category

-- | Parser for cleared status field in transaction
clearedstatus_parser :: GenParser Char st TransactionField
clearedstatus_parser = do _ <- string "C"
                          cstatus <- manyTill (noneOf ['\n','\r']) newline_or_eof
                          return $ C cstatus

                          -- | Parser for cleared status field in transaction
split_parser :: GenParser Char st (String, String, String)
split_parser = do _ <- string "S"
                  split_cat <- manyTill (noneOf ['\n','\r']) newline_or_eof
                  _ <- string "E"
                  split_merchant <- manyTill (noneOf ['\n','\r']) newline_or_eof
                  _ <- string "$"
                  split_amount <- manyTill (noneOf ['\n','\r']) newline_or_eof
                  return $ (split_cat, split_merchant, split_amount)

splits_parser :: GenParser Char st TransactionField 
splits_parser = do splits <- manyTill (split_parser) $ lookAhead (noneOf ['S'])
                   return $ S splits

-- | List of parsers for all transaction fields
transactionfield_parsers :: [ GenParser Char st TransactionField ]
transactionfield_parsers = [ date_parser, description_parser, text_parser
                           , balance_parser, category_parser, investmentaction_parser
                           , clearedstatus_parser, splits_parser ]

parse_date s = fromMaybe (Date.fromGregorian 1990 1 1) $ parseTimeM True defaultTimeLocale "%d/%m/%Y" s

parse_clearedStatus ""  = NotCleared
parse_clearedStatus "*" = Cleared
parse_clearedStatus "c" = Cleared
parse_clearedStatus "X" = Reconciled
parse_clearedStatus "R" = Reconciled

{- **************************************************
    transactions
*************************************************** -}
-- | Parser for one whole transaction
transaction_parser :: GenParser Char st Transaction
transaction_parser = do fields <- manyTill (choice transactionfield_parsers) (try $ lookAhead $ seperator_parser)
                        return $ foldl fieldToTransaction 
                                       Transaction { _date = Date.fromGregorian 1990 1 1
                                                   , _payee = ""
                                                   , _memo = ""
                                                   , _amount = 0.0
                                                   , _investmentAction = Nothing
                                                   , _category = Nothing
                                                   , _clearedStatus = NotCleared
                                                   , _splits = []
                                                   }
                                       fields
              where fieldToTransaction trans (D _d)   = trans & date     .~ (parse_date _d) 
                    fieldToTransaction trans (P _p)   = trans & payee    .~ _p 
                    fieldToTransaction trans (M _m)   = trans & memo     .~ _m 
                    fieldToTransaction trans (T _a)   = trans & amount   .~ (read $ filter (/= ',') _a) 
                    fieldToTransaction trans (L cat)  = trans & category .~ Just cat 
                    fieldToTransaction trans (N iact) = trans & investmentAction .~ Just iact
                    fieldToTransaction trans (C st)   = trans & clearedStatus .~ parse_clearedStatus st 
                    fieldToTransaction trans (S _ss)  = trans & splits .~ (map (\(_c,_m,_a)-> (Just _c,_m,read $ filter (/= ',') _a)) _ss)

-- | Parser for transaction seperator '^'
seperator_parser :: GenParser Char st ()
seperator_parser = do  _ <- string "^" 
                       _ <- newline_or_eof
                       return ()

-- | Parser for a list of transactions
transactions_parser :: GenParser Char st [Transaction]
transactions_parser = sepEndBy transaction_parser seperator_parser


{- **************************************************
    Qif file
*************************************************** -}
-- | Parser for type of qif (Bank, CCard, etc.)
type_parser :: GenParser Char st AccountType
type_parser = do _ <- string "!Type:"
                 typeinfo <- manyTill (noneOf ['\n','\r']) newline_or_eof
                 let _accountType = case typeinfo of
                       "Cash"    -> Cash     
                       "Bank"    -> Bank     
                       "CCard"   -> CCard    
                       "Invst"   -> Invst    
                       "Oth A"   -> OthA     
                       "Oth L"   -> OthL     
                       "Invoice" -> Invoice  
                 return _accountType 

-- | Parser for a qif file  
qif_file_parser :: GenParser Char st Qif
qif_file_parser = do typeinfo     <- type_parser
                     transactions <- transactions_parser
                     return Qif{ accountType = typeinfo, transactions = transactions }

-- | obtain qif from string
qifFromString s = fromRight (Qif{accountType = Bank, transactions = []}) $ parse qif_file_parser "(unknown)" s

-- | IO Monad helper function: reads and parses qif file
-- * if parsing successful returns qif data structure, otherwise returns "empty" qif
parse_qif_file filename = do contents <- readFile filename
                             let parsed_qif = parse qif_file_parser filename contents
                             if (isRight parsed_qif) 
                             then return $ fromRight (error "impossible") parsed_qif
                             else fail $ "Parsing failure: " ++ (show $ fromLeft (error "impossible") parsed_qif)


{- **************************************************
    Helper functions
*************************************************** -}
-- | test out parser on string
test_parser p s = parse p "(unknown)" s

-- | parses newline but throws away '\n'
newline_skip = do _ <- newline
                  return ()

-- | skips newline but also succeeds at end-of-file
newline_or_eof = choice [newline_skip, eof]

-- | quick and dirty Either exception handling with default value
isRight (Right x) = True
isRight _         = False

fromRight d (Right x) = x
fromRight d _         = d

fromLeft d (Left x) = x
fromLeft d _        = d


