{-# LANGUAGE LambdaCase #-}
module Task3 where

import Data.Text ( Text )
import GHC.Natural ( Natural )

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

data Action
  = FlipCoin (FlipOutcome -> Action)
  | DrawCard (Maybe Card -> Action)
  | QueryAttached ([Card] -> Action)
  | Damage Natural

drawN :: Natural -> ([Card] -> Action) -> Action
drawN n next = go n []
  where
    go n acc
      | n <= 0    = next (reverse acc)
      | otherwise = DrawCard $ \case
                      Nothing -> next (reverse acc)
                      Just c  -> go (n - 1) (c : acc)