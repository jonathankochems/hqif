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
import Data.Functor.Identity    

-- Control
import Control.Applicative

{-*********************************************
 * Display functions
 *********************************************-}
displayQif :: Qif -> String
displayQif = unlines . go
  where go :: Qif -> [String]
        go qif = displayAccountType (accountType qif) 
                  : (foldl (\ss s-> ss ++ s++["^"]) [] . map displayTransaction $ transactions qif)

displayTransaction :: Transaction -> [String]
displayTransaction trans = ["P" ++ p, "T" ++ a, "D" ++ d, "M" ++ _memo, "C" ++ clr] 
                ++ maybe [] (\_a -> ["N" ++ _a]) act 
                ++ maybe [] (\_c -> ["L" ++ _c]) cat
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

displayClearedStatus :: ClearedStatus -> String
displayClearedStatus NotCleared = ""
displayClearedStatus Cleared    = "c"
displayClearedStatus Reconciled = "R"

displayDate :: Date.Day -> String
displayDate = formatTime defaultTimeLocale "%d/%m/%Y"

{-*********************************************
 * Lens helpers
 *********************************************-}
(^.) :: t -> ((a -> Const a b1) -> t -> Const a1 b) -> a1
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}
(.~) :: ((t -> Identity a1) -> a -> Identity c) -> a1 -> a -> c
l .~ b = runIdentity . l (\_ -> Identity b)
{-# INLINE (.~) #-}
infixr 8 &
(&) :: t1 -> (t1 -> t) -> t
x & f = f x
{-# INLINE (&) #-}

{-*********************************************
 * helpers
 *********************************************-}
applyToAmount :: (Float -> Float) -> Transaction -> Transaction
applyToAmount f x = x & amount .~ f (x ^. amount)

{- **************************************************
    Parsers for transaction fields
*************************************************** -}
-- | Helper type to tag transaction field data
data TransactionField =   D String | P String | M String 
                        | T String | N String | L String
                        | C String | S [(String, String, String)]


-- | Parser for date field in transaction
dateParser :: GenParser Char st TransactionField
dateParser = do _ <- string "D"
                _date <- manyTill (noneOf ['\n','\r']) newlineOrEof
                return $ D _date

-- | Parser for description field in transaction
descriptionParser :: GenParser Char st TransactionField
descriptionParser = do _ <- string "P"
                       description <- manyTill (noneOf ['\n','\r']) newlineOrEof
                       return $ P description

-- | Parser for text field in transaction
textParser :: GenParser Char st TransactionField
textParser = do _ <- string "M"
                text <- manyTill (noneOf ['\n','\r']) newlineOrEof
                return $ M text

-- | Parser for balance field in transaction
balanceParser :: GenParser Char st TransactionField
balanceParser = do _ <- string "T"
                   balance <- manyTill (noneOf ['\n','\r']) newlineOrEof
                   return $ T balance

-- | Parser for investment action field in transaction
investmentactionParser :: GenParser Char st TransactionField
investmentactionParser = do _ <- string "N"
                            action <- manyTill (noneOf ['\n','\r']) newlineOrEof
                            return $ N action

-- | Parser for investment action field in transaction
categoryParser :: GenParser Char st TransactionField
categoryParser = do _ <- string "L"
                    _category <- manyTill (noneOf ['\n','\r']) newlineOrEof
                    return $ L _category


--TODO(JAK): write test for Halifax Feb 2017 Interest transaction that gets parsed as 2.00 rather than 0.02

-- | Parser for cleared status field in transaction
clearedstatusParser :: GenParser Char st TransactionField
clearedstatusParser = do _ <- string "C"
                         cstatus <- manyTill (noneOf ['\n','\r']) newlineOrEof
                         return $ C cstatus

-- | Parser for cleared status field in transaction
splitParser :: GenParser Char st (String, String, String)
splitParser = do _ <- string "S"
                 split_cat <- manyTill (noneOf ['\n','\r']) newlineOrEof
                 _ <- string "E"
                 split_merchant <- manyTill (noneOf ['\n','\r']) newlineOrEof
                 _ <- string "$"
                 split_amount <- manyTill (noneOf ['\n','\r']) newlineOrEof
                 return (split_cat, split_merchant, split_amount)

splitsParser :: GenParser Char st TransactionField 
splitsParser = do _splits <- manyTill splitParser $ lookAhead (noneOf ['S'])
                  return $ S _splits

-- | List of parsers for all transaction fields
transactionfieldParsers :: [ GenParser Char st TransactionField ]
transactionfieldParsers = [ dateParser, descriptionParser, textParser
                          , balanceParser, categoryParser, investmentactionParser
                          , clearedstatusParser, splitsParser 
                          ]

parseDate :: String -> Date.Day
parseDate s = fromMaybe (Date.fromGregorian 1990 1 1) $ parseTimeM True defaultTimeLocale "%d/%m/%Y" s

parseClearedStatus :: String -> ClearedStatus
parseClearedStatus ""  = NotCleared
parseClearedStatus "*" = Cleared
parseClearedStatus "c" = Cleared
parseClearedStatus "X" = Reconciled
parseClearedStatus "R" = Reconciled
parseClearedStatus _s  = error $ "Unexpected cleared status: " ++ _s

{- **************************************************
    transactions
*************************************************** -}
-- | Parser for one whole transaction
transactionParser :: GenParser Char st Transaction
transactionParser = do fields <- manyTill (choice transactionfieldParsers) (try $ lookAhead seperatorParser)
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
              where fieldToTransaction trans (D _d)   = trans & date     .~ parseDate _d
                    fieldToTransaction trans (P _p)   = trans & payee    .~ _p 
                    fieldToTransaction trans (M _m)   = trans & memo     .~ _m 
                    fieldToTransaction trans (T _a)   = trans & amount   .~ read (filter (/= ',') _a)
                    fieldToTransaction trans (L cat)  = trans & category .~ Just cat 
                    fieldToTransaction trans (N iact) = trans & investmentAction .~ Just iact
                    fieldToTransaction trans (C st)   = trans & clearedStatus .~ parseClearedStatus st 
                    fieldToTransaction trans (S _ss)  = trans & splits .~ map (\(_c,_m,_a)-> (Just _c,_m,read $ filter (/= ',') _a)) _ss

-- | Parser for transaction seperator '^'
seperatorParser :: GenParser Char st ()
seperatorParser = do _ <- string "^" 
                     _ <- newlineOrEof
                     return ()

-- | Parser for a list of transactions
transactionsParser :: GenParser Char st [Transaction]
transactionsParser = sepEndBy transactionParser seperatorParser


{- **************************************************
    Qif file
*************************************************** -}
-- | Parser for type of qif (Bank, CCard, etc.)
typeParser :: GenParser Char st AccountType
typeParser = do _ <- string "!Type:"
                typeinfo <- manyTill (noneOf ['\n','\r']) newlineOrEof
                let _accountType = case typeinfo of
                      "Cash"    -> Cash     
                      "Bank"    -> Bank     
                      "CCard"   -> CCard    
                      "Invst"   -> Invst    
                      "Oth A"   -> OthA     
                      "Oth L"   -> OthL     
                      "Invoice" -> Invoice 
                      _other    -> error $ "unexpected account type string:" ++ _other 
                return _accountType 

-- | Parser for a qif file  
qifFileParser :: GenParser Char st Qif
qifFileParser = do typeinfo     <- typeParser
                   _transactions <- transactionsParser
                   return Qif{ accountType = typeinfo, transactions = _transactions }

-- | obtain qif from string
qifFromString :: String -> Qif
qifFromString s = fromRight Qif{accountType = Bank, transactions = []} $ parse qifFileParser "(unknown)" s

-- | IO Monad helper function: reads and parses qif file
-- * if parsing successful returns qif data structure, otherwise returns "empty" qif
parseQifFile :: FilePath -> IO Qif
parseQifFile filename = do contents <- readFile filename
                           let parsed_qif = parse qifFileParser filename contents
                           if isRight parsed_qif
                           then return $ fromRight (error "impossible") parsed_qif
                           else fail $ "Parsing failure: " ++ show (fromLeft (error "impossible") parsed_qif)


{- **************************************************
    Helper functions
*************************************************** -}
-- | test out parser on string
testParser :: GenParser Char () a -> String -> Either ParseError a 
testParser p = parse p "(unknown)"

-- | parses newline but throws away '\n'
newlineSkip :: GenParser Char st ()
newlineSkip = do _ <- newline
                 return ()

-- | skips newline but also succeeds at end-of-file
newlineOrEof :: GenParser Char st ()
newlineOrEof = choice [newlineSkip, eof]

-- | quick and dirty Either exception handling with default value
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight d _         = d

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft d _        = d


