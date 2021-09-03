{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PoC.Animal where

import Control.Applicative
import Control.Monad
import Type.Reflection
import Prelude

-- * preliminary types

data Sound = Miaow | Wafu
  deriving (Eq, Show)

data CatColour = Orange | White | Black
  deriving (Eq, Show)

data Species = FelisCatus | CanisFamiliaris | Cryptodira
  deriving (Eq, Show)

-- * Classification types (classes)

class Animal a where
  getName :: a -> String
  getSpecies :: a -> Species
  getAge :: a -> Float

  -- | Heterogeneous Animal constructor
  animalAsOf :: a -> SomeAnimal

class (Animal a) => Mammal a where
  isFurry :: a -> Bool
  makesSound :: a -> Sound

class (Animal a) => Winged a where
  flys :: a -> Bool
  feathered :: a -> Bool

-- * domain types

data Cat = Cat
  { cat_name :: String,
    cat_age :: Float,
    cat_colour :: CatColour,
    cat_is_trying_to_kill_me :: Bool
  }

instance Animal Cat where
  getName = cat_name
  getSpecies = const FelisCatus
  getAge = cat_age

  animalAsOf =
    SomeAnimal $
      AnimalType @Cat
        id
        {- HLINT ignore "Use const" -}
        {- 'const' really won't work here, with strange error:
            Could not deduce (Winged Cat) from the context
        -}
        -- (const mzero)
        (\_ -> mzero)

instance Mammal Cat where
  isFurry _ = True
  makesSound _ = Miaow

data Tortoise = Tortoise
  { tortoise_name :: String,
    tortoise_age :: Float
  }

instance Animal Tortoise where
  getName = tortoise_name
  getSpecies = const Cryptodira
  getAge = tortoise_age

  animalAsOf =
    SomeAnimal $
      AnimalType @Tortoise
        (\_ -> mzero)
        (\_ -> mzero)

-- * comprehension types

data AnimalType a = AnimalType
  { with'mamal'type ::
      forall m r.
      (MonadPlus m) =>
      (forall a'. (a' ~ a, Mammal a') => m r) ->
      m r,
    with'winged'type ::
      forall m r.
      (MonadPlus m) =>
      (forall a'. (a' ~ a, Winged a') => m r) ->
      m r
  }

data SomeAnimal = forall a. (Animal a) => SomeAnimal (AnimalType a) a

-- * demo usage

-- | Polymorphic Animal examination
vet :: SomeAnimal -> IO ()
vet (SomeAnimal t a) = do
  -- a's 'Animal' instance is apparent, which is witnessed even statically
  putStrLn $
    "Let's see what " <> getName a <> " really is ..."
  putStrLn $
    "It is a " <> show (getSpecies a) <> "."

  (<|> putStrLn "We know it's not a mammal.") $
    with'mamal'type t $ do
      -- here GHC can witness a's 'Mammal' instance, dynamically
      putStrLn $
        "It's a mammal that "
          <> if isFurry a then "furry." else " with no fur."
      putStrLn $
        "It says \"" <> show (makesSound a) <> "\"."

  (<|> putStrLn "We know it's not winged.") $
    with'winged'type t $ do
      -- here GHC can witness a's 'Winged' instance, dynamically
      putStrLn $
        "It's winged "
          <> if flys a then "and can fly." else "but can't fly."
      putStrLn $
        "It " <> if feathered a then "does" else "doesn't" <> " have feather."

main :: IO ()
main = do
  vet $ animalAsOf $ Cat "Doudou" 1.2 Orange False
  vet $ animalAsOf $ Tortoise "Khan" 101.5
