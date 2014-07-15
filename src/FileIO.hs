{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, DeriveFunctor #-}
import Control.Eff
import System.IO (Handle, IOMode(..))
import System.IO as IO
import System.FilePath (FilePath)
import Data.Typeable

data F result v = Read (Char -> result) 
  deriving (Typeable, Functor)

readChar :: (Typeable result, Member (F result) r) => (Char -> result) -> Eff r a
readChar f = send $ \k -> inj (Read f)

runOpenFile :: (Typeable result) => FilePath -> Eff (F result :> ()) result -> IO result
runOpenFile path action = do 
    h <- IO.openFile path ReadMode
    loop h (admin action)
  where
    loop :: (Typeable result) => Handle -> VE (F result :> ()) result -> IO result
    loop h (Val x) = return x
    loop h (E u) = prjForce u (doRead h)

    doRead :: Handle -> F result v -> IO result
    doRead h (Read f) = hGetChar h >>= return . f

main = do
  x <- runOpenFile "Setup.hs" (readChars id) :: IO Char
  putStrLn (show x)
  
