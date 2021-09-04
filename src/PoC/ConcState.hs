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

seekTail ::
  forall a.
  (a -> Timestamp -> a -> Timestamp -> a) ->
  ValueSink a ->
  STM (ValueSink a, Maybe (ValueNode a))
seekTail f sink = go sink Nothing
  where
    go ::
      ValueSink a ->
      Maybe (ValueNode a) ->
      STM (ValueSink a, Maybe (ValueNode a))
    go ref prevNode =
      tryReadTMVar ref >>= \case
        Nothing -> return (ref, prevNode)
        Just self@(ValueNode spotVal spotTs nxt) ->
          go nxt $
            Just
              self
                { node'value = case prevNode of
                    Nothing -> spotVal
                    Just (ValueNode prevVal prevTs _prevNxt) ->
                      f prevVal prevTs spotVal spotTs
                }

updateValue ::
  forall a m.
  MonadIO m =>
  (Maybe (a, Timestamp) -> m (a, Timestamp)) ->
  ValueSink a ->
  m ()
updateValue f sink = do
  (tailRef, tailNode) <- liftIO $ atomically $ seekTail justLatest sink
  case tailNode of
    Nothing -> do
      (myVal, myTs) <- f Nothing
      liftIO $
        atomically $ do
          nxt <- newEmptyTMVar
          void $ tryPutTMVar tailRef $ ValueNode myVal myTs nxt
    Just (ValueNode spotVal spotTs spotNxt) -> do
      (myVal, myTs) <- f $ Just (spotVal, spotTs)
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

      liftIO $ atomically $ putAsNewTailOrDiscard spotNxt
  where
    justLatest :: (a -> Timestamp -> a -> Timestamp -> a)
    justLatest _prevVal _prevTs spotVal _spotTs = spotVal

-- Each concurrent thread is supposed to have its local 'ValueSink' reference
-- "cached" over time, but keep in mind that for any such thread who is slow
-- in unfolding the value stream, the historical values will pile up in heap.
