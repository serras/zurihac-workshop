{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language DeriveGeneric, DeriveAnyClass #-}
module Operational1 where

import Control.Monad
import Control.Monad.Loops
import Data.List (genericLength)
import Data.Text (Text)
import GHC.Natural
import GHC.Generics
import System.Random
import System.Random.Stateful

data Energy = Colorless
            | Grass | Fire | Water
            | Lightning | Fighting | Psychic
            | Darkness | Metal | Dragon

data Card = PokemonCard { name    :: Text
                        , typ     :: Energy
                        , hp      :: Natural
                        , attacks :: [Attack] }
          | EnergyCard  { typ     :: Energy }

data Attack = Attack { attackName :: Text
                     , cost       :: [Energy]
                     , damage     :: Natural }

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
  DrawCard :: Action (Maybe Card)
  QueryAttached :: Action [Card]

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
  _ -> undefined

-- | Define Pikachu's "Iron Tail" attack
--
--   > Flip a coin until you get tails.
--   > This attack does 30 damage for each heads.
ironTailAction :: Program Action Natural
ironTailAction = do
  outcome <- perform FlipCoin
  case outcome of
    Tails -> pure 0
    Heads -> (30 +) <$> ironTailAction

ironTailAction2 :: Program Action Natural
ironTailAction2 = do
  heads <- unfoldWhileM (== Heads) (perform FlipCoin)
  pure $ 30 * genericLength heads

-- | Draw 'n' cards.
--
--   The resulting list may have fewer cards
--   than requested, if there were not enough.
drawN :: Natural -> Program Action [Card]
drawN n = _

-- | Define "Ice Bonus" attacj
--
--   > Discard a Water Energy card from your hand.
--   > If you do, draw 3 cards.
iceBonusAction :: Program Action Natural
iceBonusAction = _