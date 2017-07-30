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
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, Rank2Types #-}
module Data.Qif.Sample where

-- Control
import Control.Monad(forM, when, void)
import Control.Arrow(first, second)

-- Data
import qualified Data.Qif as HQif
import Data.Maybe(isJust, fromMaybe, fromJust)
import Data.List(elemIndex, sort, init, inits, sort)
import Data.Time.Calendar(fromGregorian)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Trie as Trie

-- Test
import Test.QuickCheck

import Text.Parsec ( parse, manyTill, anyChar, string
                   , many, digit, letter, spaces
                   , choice, try, eof, lookAhead
                   , optionMaybe, Stream(..), SourcePos, unknownError
                   )
import Text.Parsec.String (GenParser)
import qualified Text.Parsec as Parsec
import Text.Parsec.Pos(updatePosString)
import Text.Parsec.Error(setErrorMessage, newErrorMessage, newErrorUnknown, Message(..))
import Unsafe.Coerce(unsafeCoerce)

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

---------------------------------------------------------------------------------------------------
-- combinators
---------------------------------------------------------------------------------------------------
-- | partial_string' ensures we consume at least one symbol if successful
partialString' :: String -> GenParser Char st (String, Bool)
partialString'= partialTokens' show updatePosString 

newtype ParsecT s u m a
    = ParsecT {unParser :: forall b .
                 Parsec.State s u
              -> (a -> Parsec.State s u -> Parsec.ParseError -> m b) -- consumed ok
              -> (Parsec.ParseError -> m b)                          -- consumed err
              -> (a -> Parsec.State s u -> Parsec.ParseError -> m b) -- empty ok
              -> (Parsec.ParseError -> m b)                          -- empty err
              -> m b
             }
myParsecT :: (forall b .
                 Parsec.State s u
              -> (a -> Parsec.State s u -> Parsec.ParseError -> m b) -- consumed ok
              -> (Parsec.ParseError -> m b)                          -- consumed err
              -> (a -> Parsec.State s u -> Parsec.ParseError -> m b) -- empty ok
              -> (Parsec.ParseError -> m b)                          -- empty err
              -> m b) -> Parsec.ParsecT s u m a
myParsecT x = unsafeCoerce $ ParsecT x

fromStrings :: [String] -> Trie.Trie ()
fromStrings xs = Trie.fromList . map (first ByteString.pack) $ zip xs (repeat ())

decompTrie :: Char -> Trie.Trie () -> (Maybe (), Trie.Trie ())
decompTrie c = Trie.lookupBy (\x y -> (x,y)) (ByteString.pack [c])

representative :: Trie.Trie b -> String
representative t = head . map (ByteString.unpack . fst) $ Trie.toList t

failed :: (Maybe t, Trie.Trie a) -> Bool
failed (Nothing,t) = Trie.null t
failed _           = False

partial :: (Maybe t, Trie.Trie a) -> Bool
partial (Nothing,t) | Trie.null t = False
                    | otherwise   = True
partial _           = False

finished :: (Maybe (), Trie.Trie a) -> Bool
finished (Just (),t) | Trie.null t = True
                     | otherwise   = error "Trie contains prefixes, ambiguities are not handled."
finished _         = False

tokenTrie :: (Stream String m Char)
       => ([String] -> String)       -- Pretty print a list of tokens
       -> (SourcePos -> String -> SourcePos)
       -> Trie.Trie ()          -- List of tokens to parse
       -> Parsec.ParsecT String u m (String, Bool)
{-# INLINE tokenTrie #-}
tokenTrie showTokens nextposs trie
  | Trie.null trie 
      = myParsecT $ \s _ _ eok _ ->
          eok ([],True) s $ unknownError s
  | otherwise
   = myParsecT $ \(Parsec.State input pos u) cok cerr _eok eerr -> 
    let errEof      = errMsg showTokens (map (ByteString.unpack . fst) $ Trie.toList trie) "" pos
        errExpect x = errMsg showTokens (map (ByteString.unpack . fst) $ Trie.toList trie) (showTokens [x]) pos 
        walk ps trieRes@(_,_trie) rs 
          | failed trieRes && length ps == 1 = eerr $ errExpect ps
          | failed trieRes                   = cerr $ errExpect ps
          | finished trieRes                 = ok ps rs
          | otherwise                        = do
          sr <- uncons rs
          case sr of
            Nothing     -> eerr errEof
            Just (x :: Char,xs) -> walk (x:ps) (decompTrie x _trie) xs

        ok ps rs = let pos' = nextposs pos $ reverse ps
                       s'   = Parsec.State rs pos' u
                   in cok ({-TODO(JAK):FIX needs to be full string-}reverse ps,True) s' (newErrorUnknown pos')
    in do
        sr <- uncons input
        case sr of
            Nothing     -> eerr errEof
            Just (x,xs) -> walk [x] (decompTrie x trie) xs

partialTokenTrie :: (Stream String m Char)
       => ([String] -> String)       -- Pretty print a list of tokens
       -> (SourcePos -> String -> SourcePos)
       -> Trie.Trie ()          -- List of tokens to parse
       -> Parsec.ParsecT String u m (String,Bool)
{-# INLINE partialTokenTrie #-}
partialTokenTrie showTokens nextposs trie
 | Trie.null trie
    = myParsecT $ \s _ _ eok _ ->
          eok ([],True) s $ unknownError s
 | otherwise
    = myParsecT $ \(Parsec.State input pos u) cok cerr _eok eerr -> 
    let errEof      = errMsg showTokens (map (ByteString.unpack . fst) $ Trie.toList trie) "" pos
        errExpect x = errMsg showTokens (map (ByteString.unpack . fst) $ Trie.toList trie) (showTokens [x]) pos 
        walk ps trieRes@(_,_trie) rs 
          | failed trieRes && length ps == 1 = eerr $ errExpect ps
          | failed trieRes                   = cerr $ errExpect ps
          | finished trieRes                 = ok ps rs
          | otherwise                        = do
          sr <- uncons rs
          case sr of
            Nothing     -> pok (reverse ps) _trie rs
            Just (x,xs) -> walk (x:ps) (decompTrie x _trie) xs

        pok ps t rs = let pos' = nextposs pos ps
                          s'   = Parsec.State rs pos' u
                      in cok (ps ++ representative t, False) s' (newErrorUnknown pos')
        ok ps rs = let pos' = nextposs pos $ reverse ps
                       s'   = Parsec.State rs pos' u
                   in cok (reverse ps,True) s' (newErrorUnknown pos')
    in do
        sr <- uncons input
        case sr of
            Nothing     -> eerr errEof
            Just (x,xs) -> walk [x] (decompTrie x trie) xs

partialTokens' :: (Stream s m t, Eq t)
       => ([t] -> String)      -- Pretty print a list of tokens
       -> (SourcePos -> [t] -> SourcePos)
       -> [t]                  -- List of tokens to parse
       -> Parsec.ParsecT s u m ([t],Bool)
{-# INLINE partialTokens' #-}
partialTokens' _ _ []
    = myParsecT $ \s _ _ eok _ ->
          eok ([],True) s $ unknownError s
partialTokens' showTokens nextposs tts@(tok:toks)
    = myParsecT $ \(Parsec.State input pos u) cok cerr _eok eerr -> 
    let errEof      = errMsg showTokens tts "" pos
        errExpect x = errMsg showTokens tts (showTokens [x]) pos
        walk _ []     rs = ok rs
        walk ps (t:ts) rs = do
          sr <- uncons rs
          case sr of
            Nothing                 -> fok (reverse ps) rs
            Just (x,xs) | t == x    -> walk (t:ps) ts xs
                        | otherwise -> cerr $ errExpect x
        fok ps rs = let pos' = nextposs pos ps
                        s'   = Parsec.State rs pos' u
                    in cok (tts,False) s' (newErrorUnknown pos')
        ok rs = let pos' = nextposs pos tts
                    s'   = Parsec.State rs pos' u
                in cok (tts,True) s' (newErrorUnknown pos')
    in do
        sr <- uncons input
        case sr of
            Nothing         -> eerr errEof
            Just (x,xs)
                | tok == x  -> walk [x] toks xs
                | otherwise -> eerr $ errExpect x

{-# INLINE errMsg #-}
errMsg :: (t -> String) -> t -> String -> SourcePos -> Parsec.ParseError
errMsg showTokens etts xtts pos = setErrorMessage (Expect (showTokens etts))
                                   (newErrorMessage (SysUnExpect xtts) pos)

partialTokens :: (Stream s m t, Eq t)
       => ([t] -> String)      -- Pretty print a list of tokens
       -> (SourcePos -> [t] -> SourcePos)
       -> [t]                  -- List of tokens to parse
       -> Parsec.ParsecT s u m ([t],Bool)
{-# INLINE partialTokens #-}
partialTokens _ _ []
    = myParsecT $ \s _ _ eok _ ->
          eok ([],True) s $ unknownError s
partialTokens showTokens nextposs tts@(tok:toks)
    = myParsecT $ \(Parsec.State input pos u) cok cerr _eok eerr -> 
    let errExpect x = errMsg showTokens tts (showTokens [x]) pos
        walk _  []     rs = ok rs
        walk ps (t:ts) rs = do
          sr <- uncons rs
          case sr of
            Nothing                 -> pok (reverse ps) rs
            Just (x,xs) | t == x    -> walk (t:ps) ts xs
                        | otherwise -> cerr $ errExpect x

        pok ps rs = let pos' = nextposs pos ps
                        s'   = Parsec.State rs pos' u
                in cok (tts,False) s' (newErrorUnknown pos')
        ok rs = let pos' = nextposs pos tts
                    s'   = Parsec.State rs pos' u
                in cok (tts,True) s' (newErrorUnknown pos')
    in do
        sr <- uncons input
        case sr of
            Nothing         -> pok [] input 
            Just (x,xs)
                | tok == x  -> walk [x] toks xs
                | otherwise -> eerr $ errExpect x


-- | partial_string'' consideres eof as partial 
partial_string'' :: String -> GenParser Char st (String, Bool)
partial_string'' s = choice $ (try . fmap (\x -> (x,True)) $ string s) : ss 
  where ss          = map _partial . reverse . init $ inits s
        _partial s' = try $ string s' >> eof >> return (s,False)

partialCharParser :: GenParser Char st Char -> GenParser Char st (Maybe Char)
partialCharParser p = do atEof <- choice [const False <$> lookAhead p, const True <$> lookAhead eof]
                         if atEof 
                         then eof >> return Nothing
                         else Just <$> p 


partialString :: String -> GenParser Char st String
partialString s = fst <$> partialString' s 

partialStringEof :: String -> GenParser Char st String
partialStringEof s = do 
  (_s,_full) <- partialString' s
  when _full eof
  return s


---------------------------------------------------------------------------------------------------
-- parsers
---------------------------------------------------------------------------------------------------
parseDescription :: String -> Either Parsec.ParseError Transaction
parseDescription = parse descriptionParser "(unknown)"

descriptionParser :: GenParser Char st Transaction 
descriptionParser = choice [try fdesc, pdesc] 
  where fdesc = do merchant <- manyTill anyChar (try $ lookAhead $ do _ <- string " "
                                                                      choice [ void dateParser
                                                                             , void typeParser])
                   _ <- string " "
                   _date     <- optionMaybe . try $ dateParser
                   if isJust _date && snd (fromJust _date) 
                   then do
                     let __date = fst $ fromJust _date
                     spaces
                     type_    <- typeParser
                     return Transaction{ transactionType = type_
                                       , remainder = merchant
                                       , date      = __date
                                       }
                   else return Transaction{ transactionType = Nothing
                                          , remainder = merchant
                                          , date      = Nothing
                                          }
        pdesc = do merchant <- manyTill anyChar eof 
                   return Transaction{ transactionType = Nothing
                                     , remainder = merchant
                                     , date      = Nothing
                                     }

dateParser :: GenParser Char st (Maybe (Int,String),Bool)
dateParser = do (_,_full)     <- partialString' "ON "
                if _full 
                then do
                  day_string <- many digit 
                  if length day_string >= 2 -- shall we fail if >2?
                  then do
                    let day = read day_string
                    (_,_full') <- partial_string'' " "
                    if _full'
                    then do 
                      (x :: Maybe Char  ) <- partialCharParser letter
                      (y :: Maybe Char  ) <- partialCharParser letter
                      (z :: Maybe Char  ) <- partialCharParser letter
                      let m = do _x <- x
                                 _y <- y
                                 _z <- z
                                 return [_x,_y,_z]
                      return (fmap (\month -> (day,month)) m, isJust m)
                    else return (Nothing, False)
                  else eof >> return (Nothing, False)
                else return (Nothing, False)  

typeParser :: GenParser Char st (Maybe String) 
typeParser = choice [ try $ do 
                       (x,_) <- partialTokenTrie show updatePosString transaction_type_trie
                       eof
                       return $ Just x
                     , eof >> return Nothing ]
  where transaction_type_trie = fromStrings transactionTypes
---------------------------------------------------------------------------------------------------


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
