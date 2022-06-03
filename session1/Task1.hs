{-# language OverloadedStrings #-}
module Task1 where

import Data.Text
import GHC.Natural

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


-- Define values for the following cards

-- | https://pokemoncard.io/card/?search=swsh1-11
grookey :: Card
grookey = _

-- | https://pokemoncard.io/card/?search=swsh8-195
goomy :: Card
goomy = _

-- | https://pokemoncard.io/card/?search=swsh4-130
eevee :: Card
eevee = _


-- | Check whether some energy cards are enough to
--   "pay" for the cost of an attack
enoughEnergy :: [Energy] -> [Card] -> Bool
enoughEnergy cost attached = _

-- Then, refine it to return the missing energy
missingEnergy :: [Energy] -> [Card] -> Maybe [Energy]
missingEnergy cost attached = _