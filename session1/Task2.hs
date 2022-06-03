{-# language LambdaCase #-}
{-# language DeriveGeneric, DeriveAnyClass #-}
module Task2 where

import GHC.Natural
import GHC.Generics
import System.Random
import System.Random.Stateful

data FlipOutcome 
  = Heads | Tails
  deriving (Generic, Finite, Uniform)

data Action
  = FlipCoin (FlipOutcome -> Action)
  | Damage Natural

surpriseAttackAction :: Action
surpriseAttackAction
  = FlipCoin $ \case Heads -> Damage 30
                     Tails -> Damage 0

-- | Define Pikachu's "Iron Tail" attack
--
--   > Flip a coin until you get tails.
--   > This attack does 30 damage for each heads.
ironTailAction :: Action
ironTailAction = _

-- | Define the randomness interpretation of 'Action'
interpretRandom :: Action -> IO Natural
interpretRandom _ = _
  where
    flipCoin :: IO FlipOutcome
    flipCoin = uniformM globalStdGen