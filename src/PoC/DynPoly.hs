module PoC.DynPoly where

-- %%
import Control.Monad (void)
import Data.Dynamic (Dynamic (..), fromDynamic, toDyn)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Proxy
import HsTyping.Shim
import Type.Reflection (eqTypeRep, typeRep, pattern App, type (:~~:) (HRefl))
import Prelude

-- %-

dynHoldEvent :: Dynamic -> Dynamic
dynHoldEvent (Dynamic trEvs monotypedEvs) =
  case trEvs of
    App trEs TypeRep ->
      case trEs `eqTypeRep` typeRep @EventSink of
        Just HRefl -> Dynamic TypeRep (holdEvent monotypedEvs)
        Nothing -> error "not an EventSink" -- to be handled properly
    _ -> error "even not a poly-type" -- to be handled properly
  where
    holdEvent :: forall a. EventSink a -> IO (TemporalValue a)
    holdEvent !evs = do
      !holder <- newIORef Nothing
      listenEvents evs $ writeIORef holder . Just
      return $ TemporalValue $ readIORef holder

data EventSink a = EventSink
  { listenEvents :: (a -> IO ()) -> IO (),
    publishEvent :: a -> IO ()
  }

newtype TemporalValue a = TemporalValue {readTemporalValue :: IO (Maybe a)}

newEventSink :: forall a. IO (EventSink a)
newEventSink = do
  !listeners <- newIORef []
  let listen listener = modifyIORef' listeners (listener :)
      publish a = readIORef listeners >>= void . mapM ($ a)
  return $ EventSink listen publish

testDynHold :: IO ()
testDynHold = do
  (evs :: EventSink Int) <- newEventSink
  let !dynEvs = toDyn evs
      !dynHold = dynHoldEvent dynEvs
  !dynTs <- dynPerformIO (error "bug: dyn type mismatch?") dynHold
  case fromDynamic dynTs of
    Nothing -> error "bug: unexpected dyn result type"
    Just (ts :: TemporalValue Int) -> do
      v0 <- readTemporalValue ts
      putStrLn $ "First got " <> show v0
      publishEvent evs 3
      v1 <- readTemporalValue ts
      putStrLn $ "Then got " <> show v1

-- %%
type Float64 = Double

-- %%
newtype Float64' = Float64' {float64' :: Double}

-- %-

testDynProxy :: IO ()
testDynProxy = do
  -- %%
  let dt = Proxy :: Proxy Double
      ddt = toDyn dt

  putStrLn $ case fromDynamic ddt of
    Just (Proxy :: Proxy Float64) -> "Compatible"
    _ -> "Incompatible"
  -- %-

  -- %%
  let dt = Proxy :: Proxy Double
      ddt = toDyn dt

  putStrLn $ case fromDynamic ddt of
    Just (Proxy :: Proxy Float64') -> "Compatible"
    _ -> "Incompatible"
  -- %-

  return ()
