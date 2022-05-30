{-# language LambdaCase #-}
module Session1.Task2 where

import GHC.Natural

data FlipOutcome = Heads | Tails

data Action
  = FlipCoin (FlipOutcome -> Action)
  | Damage Natural

surpriseAttackAction :: Action
surpriseAttackAction
  = FlipCoin $ \case Heads -> Damage 30
                     Tails -> Damage 0

ironTailAction :: Action
ironTailAction = _