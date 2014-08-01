{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, DeriveFunctor #-}
{-

This is an earlier version of the OpenFile effect work; superseded
by OpenFile2.hs and friends.

-}
module OpenFile (readChar, runOpenFile, atEOF
       , readAll
       , OpenFile)

where

import Control.Eff
import System.IO (Handle, IOMode(..))
import System.IO as IO
import System.FilePath (FilePath)
import Data.Typeable
import qualified Data.Char as Char

data OpenFile v = Read (Char -> v) 
     | EOF (Bool -> v)
     | GetHandle (Handle -> v)
  deriving (Typeable, Functor)

-- | Read a character from an open file.
readChar :: (Member OpenFile r) => Eff r Char
readChar = send $ \k -> inj (GetHandle k)

-- | Read all characters from a file.
readAll :: (Member OpenFile r) => Eff r [Char]
readAll = do
  eof <- atEOF
  if not eof
  then (do
    c <- readChar
    rest <- readAll
    return (c:rest))
  else return []    
     
-- | Check if we are at EOF.
atEOF :: (Member OpenFile r) => Eff r Bool
atEOF = send $ \k -> inj (EOF k)

-- | Run a program with the OpenFile effect.
runOpenFile :: FilePath -> Eff (OpenFile :> r) result -> IO result
runOpenFile path action = do
    h <- IO.openFile path ReadMode
    loop h (admin action)
  where
    loop :: Handle -> (VE (OpenFile :> r) result) -> IO result
    loop h (Val x) = do
      hClose h
      return x
    loop h eff@(E u) = 
      case prj u of
           Just (Read k) -> do
             char <- hGetChar h
             loop h (k char)
           Just (EOF k) -> do 
             eof <- hIsEOF h
             loop h (k eof)
           Nothing -> error "Could not find OpenFile"
  
main = do
  x <- runOpenFile "Setup.hs" readAll
  putStrLn x
  x <- runOpenFile "Setup.hs" readChar
  putStrLn (show x)
  
