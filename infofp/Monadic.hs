module Monadic where

import Control.Monad (ap, liftM)
import Control.Monad.Loops
import GHC.Natural

data FlipOutcome 
  = Heads | Tails
  deriving (Eq)

data Action a
  = FlipCoin (FlipOutcome -> Action a)
  | Return a

instance Monad Action where
  return = _
  x >>= f = _

instance Applicative Action where
  pure = Return
  (<*>) = ap

instance Functor Action where
  fmap = liftM

flipCoin :: Action FlipOutcome
flipCoin = FlipCoin Return

while :: Monad m => (a -> Bool) -> m a -> m [a]
while = unfoldWhileM

-- | Define Pikachu's "Iron Tail" attack
--
--   > Flip a coin until you get tails.
--   > This attack does 30 damage for each heads.
ironTailAction :: Action Int
ironTailAction = do
  heads <- while (/= Tails) flipCoin
  return (30 * length heads)