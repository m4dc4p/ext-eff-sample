{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, DeriveFunctor #-}
module OpenFile2 (runWriteableFile, getWriteableHandle, WriteableFile
       , runReadableFile, getReadableHandle, ReadableFile)

where

import Control.Eff
import Control.Eff.Lift
import System.IO (Handle, IOMode(..))
import System.IO as IO
import System.FilePath (FilePath)
import Data.Typeable
import qualified Data.Char as Char

data WriteableFile v = GetWriteableHandle (Handle -> v)
  deriving (Typeable, Functor)

getWriteableHandle :: (Member WriteableFile r) => Eff r Handle
getWriteableHandle = send $ \k -> inj (GetWriteableHandle k)

-- | Run a program with the OpenFile effect.
runWriteableFile :: (SetMember Lift (Lift IO) r) => FilePath -> Eff (WriteableFile :> r) result -> Eff r result
runWriteableFile path action = do
    h <- lift $ IO.openFile path WriteMode
    loop h (admin action)
  where
    loop h (Val x) = do
      lift $ hClose h
      return x
    loop h eff@(E u) = handleRelay u (loop h) (writeableEff h)
    writeableEff h (GetWriteableHandle k) = loop h (k h)

data ReadableFile v = GetReadableHandle (Handle -> v)
  deriving (Typeable, Functor)

getReadableHandle :: (Member ReadableFile r) => Eff r Handle
getReadableHandle = send $ \k -> inj (GetReadableHandle k)

-- | Run a program with the OpenFile effect.
runReadableFile :: (SetMember Lift (Lift IO) r) => FilePath -> Eff (ReadableFile :> r) result -> Eff r result
runReadableFile path action = do
    h <- lift $ IO.openFile path ReadMode
    loop h (admin action)
  where
    loop h (Val x) = do
      lift $ hClose h
      return x
    loop h eff@(E u) = handleRelay u (loop h) (\(GetReadableHandle k) -> loop h (k h))

