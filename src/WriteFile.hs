{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, DeriveFunctor #-}
module WriteFile

where

import Control.Eff
import Control.Eff.Lift
import System.IO (Handle, IOMode(..))
import System.IO as IO
import System.FilePath (FilePath)
import Data.Typeable
import qualified Data.Char as Char
import OpenFile2 (getWriteableHandle, WriteableFile, runWriteableFile)

data WriteFile v = Write Char (() -> v)
     | Seek Integer (() -> v) 
     | Tell (Integer -> v)
  deriving (Typeable, Functor)

writeChar :: (Member WriteFile r) => Char -> Eff r ()
writeChar c = send $ \k -> inj (Write c k)

writeAll :: (Member WriteFile r) => String -> Eff r ()
writeAll [] = return ()
writeAll (c:cs) = do
  writeChar c
  writeAll cs

seekFile :: (Member WriteFile r) => Integer -> Eff r ()
seekFile pos = send $ \k -> inj (Seek pos k)

tellFile :: (Member WriteFile r) => Eff r Integer
tellFile = send $ \k -> inj (Tell k)

runWriteFile :: SetMember Lift (Lift IO) r => FilePath -> Eff (WriteFile :> WriteableFile :> r) result -> Eff r result
runWriteFile path action = runWriteableFile path $ do
    h <- getWriteableHandle
    loop h (admin action)
  where
    loop h (Val x) = return x
    loop h eff@(E u) = handleRelay u (loop h) (writeEff h)
    writeEff h (Write c k) = do
      lift (hPutChar h c)
      loop h (k ())
    writeEff h (Seek p k) = do
      lift (hSeek h IO.AbsoluteSeek p)
      loop h (k ())
    writeEff h (Tell k) = do
      p <- lift $ hTell h
      loop h (k p)


