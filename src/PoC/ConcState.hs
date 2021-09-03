module PoC.ConcState where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

data ValueNode a = ValueNode
  { node'value :: a,
    node'timestamp :: Timestamp,
    node'next :: ValueSink a
  }

type ValueSink a = TMVar (ValueNode a)

type Timestamp = Int

seekTail :: ValueSink a -> STM (ValueSink a, Maybe (ValueNode a))
seekTail sink = go sink Nothing
  where
    go ref ancestor =
      tryReadTMVar ref >>= \case
        Nothing -> return (ref, ancestor)
        Just self@(ValueNode _ _ nxt) -> go nxt $ Just self

updateValue ::
  forall a m.
  MonadIO m =>
  (Maybe (a, Timestamp) -> m (a, Timestamp)) ->
  ValueSink a ->
  m ()
updateValue f sink = do
  (tailRef, tailNode) <- liftIO $ atomically $ seekTail sink
  case tailNode of
    Nothing -> do
      (myVal, myTs) <- f Nothing
      liftIO $
        atomically $ do
          nxt <- newEmptyTMVar
          void $ tryPutTMVar tailRef $ ValueNode myVal myTs nxt
    Just (ValueNode seenVal seenTs seenNxt) -> do
      (myVal, myTs) <- f $ Just (seenVal, seenTs)
      newNxt <- liftIO newEmptyTMVarIO
      let newTail = ValueNode myVal myTs newNxt

          putAsNewTailOrDiscard :: ValueSink a -> STM ()
          putAsNewTailOrDiscard nodeRef =
            putTMVar nodeRef newTail `orElse` yetOther'sTail
            where
              yetOther'sTail = do
                (ValueNode _other'sVal other'sTs other'sNxt) <-
                  readTMVar nodeRef
                if other'sTs >= myTs
                  then return ()
                  else putAsNewTailOrDiscard other'sNxt

      liftIO $ atomically $ putAsNewTailOrDiscard seenNxt

-- Each concurrent thread is supposed to have its local 'ValueSink' reference
-- "cached" over time, but keep in mind that for any such thread who is slow
-- in unfolding the value stream, the historical values will pile up in heap.
