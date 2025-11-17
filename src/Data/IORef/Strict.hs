{- |
Module: Data.IORef.Strict
Description: Strict IORef with NoThunks validation

Wrapper around IORef that ensures strict evaluation and validates
no thunks are stored, preventing space leaks.
-}
module Data.IORef.Strict
  ( StrictIORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef'
  ) where

import Prelude hiding (modifyIORef', newIORef, readIORef, writeIORef)

import Data.IORef qualified as Lazy
import NoThunks.Class (NoThunks, unsafeNoThunks)

-- | Strict IORef that validates no thunks
newtype StrictIORef a = StrictIORef (Lazy.IORef a)

-- | Create new strict IORef with thunk validation
newIORef :: (NoThunks a, HasCallStack) => a -> IO (StrictIORef a)
newIORef a = do
  let thunks = unsafeNoThunks a
  case thunks of
    Nothing -> StrictIORef <$> Lazy.newIORef a
    Just err -> error $ "newIORef: thunk detected: " <> show err

-- | Read from strict IORef
readIORef :: StrictIORef a -> IO a
readIORef (StrictIORef ref) = Lazy.readIORef ref

-- | Write to strict IORef with thunk validation
writeIORef :: (NoThunks a, HasCallStack) => StrictIORef a -> a -> IO ()
writeIORef (StrictIORef ref) !x = do
  let thunks = unsafeNoThunks x
  case thunks of
    Nothing -> Lazy.writeIORef ref x
    Just err -> error $ "writeIORef: thunk detected: " <> show err

-- | Modify strict IORef with strict evaluation
modifyIORef' :: StrictIORef a -> (a -> a) -> IO ()
modifyIORef' (StrictIORef ref) f =
  Lazy.atomicModifyIORef' ref (\x -> (f x, ()))
