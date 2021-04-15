module PoC.Existential where

import Data.Dynamic
import qualified Data.Vector.Storable as VS
import Foreign
import Prelude

-- * Necessary artifacts

data Series a = Series
  { seriesLen :: IO Int,
    readSeries :: Int -> IO a
  }

data SomeArray = forall a.
  (Typeable a, VS.Storable a) =>
  SomeArray
  { arrayCap :: Int,
    arrayPtr :: ForeignPtr a
  }

class ManagedArray t where
  arrayAtTheMoment :: t -> IO SomeArray

data SomeManagedArray
  = forall t.
    (Typeable t, ManagedArray t) =>
    SomeManagedArray t

-- * Things not working

managedArrayAsSeries :: SomeManagedArray -> IO Dynamic
managedArrayAsSeries (SomeManagedArray ma) = do
  vec <- do
    SomeArray cap fp <- arrayAtTheMoment ma
    return $ VS.unsafeFromForeignPtr0 fp cap

  let len = return $ VS.length vec
      rs i = return $ vec VS.! i
  return $ toDyn $ Series len rs

-- * Things working

managedArrayAsSeries' :: SomeManagedArray -> IO Dynamic
managedArrayAsSeries' (SomeManagedArray ma) = do
  SomeArray cap fp <- arrayAtTheMoment ma
  let vec = VS.unsafeFromForeignPtr0 fp cap

  let len = return $ VS.length vec
      rs i = return $ vec VS.! i
  return $ toDyn $ Series len rs

arrayAsSeries :: SomeArray -> Dynamic
arrayAsSeries (SomeArray cap fp) = do
  let vec = VS.unsafeFromForeignPtr0 fp cap
      len = return $ VS.length vec
      rs i = return $ vec VS.! i
  toDyn $ Series len rs

data SomeColumn
  = forall a.
    (Typeable a, VS.Storable a) =>
    SomeColumn (VS.Vector a)

colAsSeries :: SomeColumn -> Dynamic
colAsSeries (SomeColumn colVec) = toDyn $ Series len rs
  where
    len = return $ VS.length colVec
    rs i = return $ colVec VS.! i

arrayAsSeries' :: SomeArray -> Dynamic
arrayAsSeries' (SomeArray cap fp) =
  colAsSeries $ SomeColumn $ VS.unsafeFromForeignPtr0 fp cap
