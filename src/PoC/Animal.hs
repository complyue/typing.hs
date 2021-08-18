module PoC.Animal where

import Type.Reflection
import Prelude

-- * preliminary types

data Sound = Miaow | Wafu
  deriving (Eq, Show)

data CatColour = Orange | White | Black
  deriving (Eq, Show)

data Species = FelisCatus | CanisFamiliaris
  deriving (Eq, Show)

-- * Classification types (classes)

class Animal a where
  getName :: a -> String
  getSpecies :: a -> Species
  getAge :: a -> Float

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

instance Mammal Cat where
  isFurry _ = True
  makesSound _ = Miaow

-- * comprehension types

data AnimalType a = AnimalType
  { animal'type'holder ::
      forall r.
      (forall a'. (a' ~ a, Animal a', Typeable a') => TypeRep a -> r) ->
      r,
    animal'type'as'of'mamal ::
      forall r.
      r ->
      (forall a'. (a' ~ a, Mammal a', Typeable a') => TypeRep a -> r) ->
      r,
    animal'type'as'of'winged ::
      forall r.
      r ->
      (forall a'. (a' ~ a, Winged a', Typeable a') => TypeRep a -> r) ->
      r
  }

data SomeAnimal = forall a. SomeAnimal a (AnimalType a)

-- * demo usage

-- | Heterogeneous Cat constructor
cat :: Cat -> SomeAnimal
cat a =
  SomeAnimal a $
    AnimalType
      ($ typeRep @Cat)
      (\na exit -> exit $ typeRep @Cat)
      (\na _exit -> na)

-- | Polymorphic Animal examination
vet :: SomeAnimal -> IO ()
vet (SomeAnimal a t) = do
  animal'type'holder t $ \(_ :: TypeRep a) -> do
    -- here GHC can witness a's 'Animal' instance, dynamically
    putStrLn $
      "Let's see what " <> getName a <> " really is ..."
    putStrLn $
      "It is a " <> show (getSpecies a) <> "."
  animal'type'as'of'mamal
    t
    (putStrLn "We know it's not a mammal.")
    $ \(_ :: TypeRep a) -> do
      -- here GHC can witness a's 'Mammal' instance, dynamically
      putStrLn $
        "It's a mammal that "
          <> if isFurry a then "furry." else " with no fur."
      putStrLn $
        "It says \"" <> show (makesSound a) <> "\"."
  animal'type'as'of'winged
    t
    (putStrLn "We know it's not winged.")
    $ \(_ :: TypeRep a) -> do
      -- here GHC can witness a's 'Winged' instance, dynamically
      putStrLn $
        "It's winged "
          <> if flys a then "and can fly." else "but can't fly."
      putStrLn $
        "It " <> if feathered a then "does" else "doesn't" <> " have feather."
