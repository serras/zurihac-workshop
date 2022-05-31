{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language DeriveGeneric, DeriveAnyClass #-}
module Session1.Operational2 where

import Control.Monad
import Control.Monad.State
import Data.List (genericLength)
import Data.Text (Text)
import GHC.Natural
import GHC.Generics
import System.Random
import System.Random.Stateful

data FlipOutcome 
  = Heads | Tails
  deriving (Eq, Generic, Finite, Uniform)

data Program instr a where
  Done   :: a -> Program instr a
  (:>>=) :: instr a
         -> (a -> Program instr b)
         -> Program instr b

data Action a where
  FlipCoin :: Action FlipOutcome

perform :: instr a -> Program instr a
perform action = action :>>= Done

instance Monad (Program instr) where
  return = Done
  Done x >>= k = k x
  (x :>>= k1) >>= k2 = x :>>= (\next -> k1 next >>= k2)

instance Applicative (Program instr) where
  pure = Done
  (<*>) = ap

instance Functor (Program instr) where
  fmap = liftM

interpret :: Monad m 
          => (forall x. instr x -> m x)
          -> Program instr a -> m a
interpret f = go
  where
    go (Done x) = return x
    go (action :>>= k) = do
      x <- f action
      go (k x)
      -- f action >>= go . k

interpretRandom :: Program Action a -> IO a
interpretRandom = interpret $ \case
  FlipCoin -> uniformM globalStdGen

interpretPure :: [FlipOutcome] -> Program Action a -> a
interpretPure outcomes = 
  flip evalState (cycle outcomes) . interpret f
  where f :: Action x -> State [FlipOutcome] x
        f FlipCoin = do
          ~(result : nexts) <- get
          put nexts
          return result

interpretPure2 :: [FlipOutcome] -> Program Action a -> a
interpretPure2 outcomes = go (cycle outcomes)
  where go :: [FlipOutcome] -> Program Action x -> x
        go _ (Done x) = x
        go (result : nexts) (FlipCoin :>>= k) =
          go nexts (k result)

--- >>> interpretPure [Heads, Heads, Tails] ironTailAction
-- 60
ironTailAction :: Program Action Natural
ironTailAction = do
  outcome <- perform FlipCoin
  case outcome of
    Tails -> pure 0
    Heads -> (30 +) <$> ironTailAction
