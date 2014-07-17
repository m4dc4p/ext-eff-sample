{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, DeriveFunctor #-}
import Control.Eff
import System.IO (Handle, IOMode(..))
import System.IO as IO
import System.FilePath (FilePath)
import Data.Typeable
import qualified Data.Char as Char

data OpenFile a v = Read (Result -> v) | EOF (Result -> v)
  deriving (Typeable, Functor)

data Result = C Char | F Bool
  deriving (Typeable) 

-- | Read a character from an open file.
readChar :: (Member (OpenFile Result) r) => Eff r Result
readChar = send $ \k -> inj (Read k `asTypeOf` (toOF k))

-- | Check if we are at EOF.
atEOF :: (Member (OpenFile Result) r) => Eff r Result
atEOF = send $ \k -> inj (EOF k `asTypeOf` (toOF k))

-- | A helper function to fix the type of "OpenFile a" in the above
-- request functions.
toOF :: (a -> v) -> OpenFile a v
toOF = error "toOF should never be evaulated"

-- | Run a program with the OpenFile effect.
runOpenFile :: FilePath -> Eff (OpenFile Result :> r) result -> IO result
runOpenFile path action = do
    h <- IO.openFile path ReadMode
    loop h (admin action)

loop :: (Typeable a) => Handle -> (VE (OpenFile a :> r) result) -> IO result
loop h (Val x) = return x
loop h eff@(E u) = 
  let -- Helper function to fix the type
      -- of the result of `prj`.
      getVE :: Union (OpenFile a :> r) (VE ((OpenFile a) :> r) result) -> Maybe (OpenFile a (VE ((OpenFile a) :> r) result))
      getVE eff = error "getVE should never be evaluated"undefined
  in case prj u `asTypeOf` getVE u of
       Just (Read k) -> do
         char <- hGetChar h
         loop h (k (C char))
       Just (EOF k) -> do 
         eof <- hIsEOF h
         loop h (k (F eof))
       Nothing -> error "Could not find OpenFile"
 
main = do
  x <- runOpenFile "Setup.hs" (do
    (C x) <- readChar
    (F eof) <- atEOF
    (C y) <- readChar
    return $ [x,y] ++ show eof)
  putStrLn (show x)
  (C x) <- runOpenFile "Setup.hs" readChar
  putStrLn (show x)
  
