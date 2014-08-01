{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, DeriveFunctor #-}
module ReadFile

where

import Control.Eff
import Control.Eff.Lift
import System.IO (Handle, IOMode(..))
import System.IO as IO
import System.FilePath (FilePath)
import Data.Typeable
import qualified Data.Char as Char
import OpenFile2 (getReadableHandle, ReadableFile, runReadableFile)

data ReadFile v = Read (Char -> v) 
     | EOF (Bool -> v)
  deriving (Typeable, Functor)

-- | Read a character from an open file.
readChar :: (Member ReadFile r) => Eff r Char
readChar = send $ \k -> inj (Read k)

readAll :: (Member ReadFile r) => Eff r String
readAll = do
  eof <- atEOF
  if eof
  then return []
  else (do
    c <- readChar
    rest <- readAll
    return (c:rest))
  
-- | Check if we are at EOF.
atEOF :: (Member ReadFile r) => Eff r Bool
atEOF = send $ \k -> inj (EOF k)

-- | Run a program that uses a file opened for reading.
runReadFile :: SetMember Lift (Lift IO) r => FilePath
     -> Eff (ReadFile :> ReadableFile :> r) result
     -> Eff r result
runReadFile path action = runReadableFile path $ do
    h <- getReadableHandle
    loop h (admin action)
  where
    loop h (Val x) = return x
    loop h eff@(E u) = handleRelay u (loop h) (readEff h)
    readEff h (Read k) = do
      char <- lift $ hGetChar h
      loop h (k char)
    readEff h (EOF k) = do 
      eof <- lift $ hIsEOF h
      loop h (k eof)
