{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, DeriveFunctor, NoMonomorphismRestriction #-}
import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Monad

import Data.Typeable

-- | Trace effect for debugging
data Trace v = Trace String (() -> v)
    deriving (Typeable, Functor)

-- | Print a string as a trace.
trace :: Member Trace r => String -> Eff r ()
trace x = send (inj . Trace x)

-- | Run a computation producing Traces.
runTrace :: Eff (Trace :> ()) w -> IO w
runTrace m = loop (admin m)
  where
    loop (Val x) = return x
    loop (E u)   = prjForce u $ \(Trace s k) -> putStrLn s >> loop (k ())

mapMdebug :: (Show a, Member Trace r) => (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug f [] = return []
mapMdebug f (h:t) = do
  trace $ "mapMdebug: " ++ show h 
  h' <- f h
  t' <- mapMdebug f t
  return (h' : t')

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

bar = do
  put "x"
  x <- ask
  put (x :: Int)
  return True

runBar1 = runState (1::Int) $ flip runReader (2::Int) $ runState "" bar 
runBar2 = runState (1::Int) $ runState "" $ runReader bar (2::Int) 

--  = runTrace $ runReader (mapMdebug f [1..5]) (10 :: Int)
tMd = runTrace $ runReader (mapMdebug f [1..5]) (10 :: Int)
  where
    f x = ask `add` return x
