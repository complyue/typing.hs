module PoC.DynPoly where

import Control.Monad (void)
import Data.Dynamic (Dynamic (..), fromDynamic, toDyn)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import HsTyping.Shim
import Type.Reflection (eqTypeRep, typeRep, pattern App, type (:~~:) (HRefl))
import Prelude

dynHoldEvent :: Dynamic -> Dynamic
dynHoldEvent (Dynamic trEvs monotypedEvs) =
  case trEvs of
    App trEs TypeRep ->
      case trEs `eqTypeRep` typeRep @EventSink of
        Just HRefl -> Dynamic TypeRep (holdEvent monotypedEvs)
        Nothing -> error "not an EventSink" -- to be handled properly
    _ -> error "even not a poly-type" -- to be handled properly
  where
    holdEvent :: forall a. EventSink a -> IO (TimeSeries a)
    holdEvent !evs = do
      !holder <- newIORef Nothing
      listenEvents evs $ writeIORef holder . Just
      return $ TimeSeries $ readIORef holder

data EventSink a = EventSink
  { listenEvents :: (a -> IO ()) -> IO (),
    publishEvent :: a -> IO ()
  }

newtype TimeSeries a = TimeSeries {readTimeSeries :: IO (Maybe a)}

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
    Just (ts :: TimeSeries Int) -> do
      v0 <- readTimeSeries ts
      putStrLn $ "First got " <> show v0
      publishEvent evs 3
      v1 <- readTimeSeries ts
      putStrLn $ "Then got " <> show v1
