{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, DeriveFunctor #-}
import Control.Eff
import System.IO (Handle, IOMode(..))
import System.IO as IO
import System.FilePath (FilePath)
import Data.Typeable
import qualified Data.Char as Char

newtype OpenFile a v = Read (a -> v)
  deriving (Typeable, Functor)

readOpenFile :: (Member (OpenFile Char) r) => Eff r Char
readOpenFile = send $ \k -> inj (Read k)

runOpenFile :: FilePath -> Eff (OpenFile Char :> ()) result -> IO result
runOpenFile path action = do 
    h <- IO.openFile path ReadMode
    loop h (admin action)

loop :: Handle -> (VE (OpenFile Char :> ()) result) -> IO result
loop h (Val x) = return x
loop h (E u) = prjForce u (doRead h)

doRead :: Handle -> OpenFile Char (VE (OpenFile Char :> ()) result) -> IO result
doRead h (Read k) = do 
  c <- hGetChar h
  loop h (k c)

main = do
  x <- runOpenFile "Setup.hs" (do
    x <- readOpenFile 
    y <- readOpenFile   
    return [x,y])
  putStrLn (show x)
  x <- runOpenFile "Setup.hs" readOpenFile :: IO Char
  putStrLn (show x)
  
