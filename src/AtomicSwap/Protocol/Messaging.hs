{- |
Module: AtomicSwap.Protocol.Messaging
Description: Message passing infrastructure for atomic swap protocol

This module provides message queue operations using MonadSTM for thread-safe
communication between Alice and Bob during the atomic swap protocol.

The polymorphic design allows testing with io-sim for deterministic execution.
-}
module AtomicSwap.Protocol.Messaging
  ( -- * Message Queue Operations
    MessageQueue
  , newMessageQueue
  , sendMessage
  , receiveMessage
  , tryReceiveMessage
  ) where

import Prelude hiding (STM, TVar, atomically, newTVar, readTVar, writeTVar)

import AtomicSwap.Prelude

--------------------------------------------------------------------------------
-- Message Queue Type ----------------------------------------------------------

-- | Thread-safe message queue for protocol communication
type MessageQueue m = TQueue m Message

--------------------------------------------------------------------------------
-- Queue Operations ------------------------------------------------------------

{- |
Create a new message queue for protocol communication.

Polymorphic over any MonadSTM instance, allowing use with IO or IOSim.

Returns: A new empty message queue
-}
newMessageQueue :: MonadSTM m => m (MessageQueue m)
newMessageQueue = atomically newTQueue

{- |
Send a message to the queue.

This operation never blocks - it always succeeds immediately.

Args:
  - queue: The message queue to send to
  - msg: The message to send
-}
sendMessage :: MonadSTM m => MessageQueue m -> Message -> m ()
sendMessage queue msg = atomically $ writeTQueue queue msg

{- |
Receive a message from the queue (blocking).

This operation blocks until a message is available.

Args:
  - queue: The message queue to receive from

Returns: The next message in the queue
-}
receiveMessage :: MonadSTM m => MessageQueue m -> m Message
receiveMessage queue = atomically $ readTQueue queue

{- |
Try to receive a message from the queue (non-blocking).

This operation returns immediately with Nothing if the queue is empty.

Args:
  - queue: The message queue to receive from

Returns: Just message if available, Nothing if queue is empty
-}
tryReceiveMessage :: MonadSTM m => MessageQueue m -> m (Maybe Message)
tryReceiveMessage queue = atomically $ tryReadTQueue queue
