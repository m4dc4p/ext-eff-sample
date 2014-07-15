{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts #-}
import Control.Eff
import Control.Eff.Trace
import Control.Eff.Reader.Strict

mapMdebug :: (Show a, Member Trace r) => (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug = undefined

add :: Monad m => m Int -> m Int -> m Int
add = undefined

tMd :: (Enum a, Show a, Num a) => (a -> Eff ((Reader Int) :> (Trace :> ())) b) -> IO [b]
tMd f = runTrace $ runReader (mapMdebug f [1..5]) (10 :: Int)
