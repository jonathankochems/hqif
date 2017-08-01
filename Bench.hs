{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Bench where

-- Criterion
import Criterion.Main
    
-- Control
import Control.Monad(forM)

-- DataMorphism
import DataMorphism.StringToRNEmbedding(partial_string)
import qualified DataMorphism.StringToRNEmbedding as Embedding

-- Text
import qualified Text.Parsec as Parsec
import Data.Maybe(fromMaybe)
import Data.Functor.Identity(Identity)

main :: IO ()
main = do defaultMain [
                 bench "partial_string" $ nfIO (partialStringBench n)
               , bench "string"         $ nfIO (stringBench n)
               , bench "partial_types"  $ nfIO (partialTypesBench n)
               , bench "base"           $ nfIO (baseBench n)
            ]
  where n     =  1000

assert :: Monad m => Bool -> m ()
assert True  = return ()
assert False = error "assertion failure"

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse p = Parsec.parse p "(unknown)"

partialStringBench :: Int -> IO ()
partialStringBench n = do rs <- forM (zip [1..n] $ cycle Embedding.months) $ \(_,m) -> do
                                  (!_rs) <- forM Embedding.months $ \m' -> do 
                                              let result = either (const False) ((==) m') $ parse (partial_string m) m' 
                                              return result
                                  return $ or _rs
                          assert $ and rs
                          return ()

partialTypesBench :: Int -> IO ()
partialTypesBench n = do rs <- forM (zip [1..n] $ cycle Embedding.transaction_types) $ \(_,t) -> do
                                 let result = either (const False) ((==) t) $ fromMaybe "" <$> parse (Embedding.type_parser) t 
                                 return result
                         assert $ and rs
                         return ()

stringBench :: Int -> IO ()
stringBench n = do rs <- forM (zip [1..n] $ cycle Embedding.months) $ \(_,m) -> do
                           (!_rs) <- forM Embedding.months $ \m' -> do 
                                       let result = either (const False) ((==) m') $ parse (Parsec.string m) m' 
                                       return result
                           return $ or _rs
                   assert $ and rs
                   return ()

baseBench :: Int -> IO ()
baseBench n = do rs <- forM (zip [1..n] $ cycle Embedding.months) $ \(_,m) -> do
                         (!_rs) <- forM Embedding.months $ \m' -> do 
                                     let result = either (const False) ((==) m') $ const (Right m) m' 
                                     return result
                         return $ or _rs
                 assert $ and rs
                 return ()
