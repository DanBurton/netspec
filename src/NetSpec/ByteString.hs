{-# LANGUAGE FlexibleInstances #-}

-- | Use Lazy 'BSL.ByteString's to send and receive messages.
-- For this module, a message is prefixed by a
-- 64-bit little-endian signed integer, indicating the length in bytes
-- of the remaining message.
module NetSpec.ByteString (
  -- * Receiving
    receive
  -- * Sending
  , (!)
  , send
  , broadcast
  -- * Re-exports
  , module I
  , module BSL
  , module IO
  ) where

import System.IO as I (Handle)
import Data.ByteString.Lazy as BSL (ByteString)
import Control.Monad.IO.Class as IO (MonadIO, liftIO)

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import Data.Foldable as F
import System.IO (hFlush)
import Data.Binary.Get
import Data.Binary.Put


infix 2 !

class CanSend h where
  -- | The staple for sending a message.
  -- @!@ is typeclassed so that you can
  -- 'send' or 'broadcast' using the same simple syntax.
  -- The @CanSend@ typeclass is not exposed.
  -- Instances of CanSend include 'I.Handle'
  -- and @Traversable t => t Handle@.
  -- 
  -- @!@ produces an @IO@ action lifted into any 'IO.MonadIO',
  -- so can be used without the extra cruft of 'IO.liftIO'
  -- for most monad stacks. @!@ is declared as @infix 2@.
  -- 
  -- Usage:
  -- 
  -- > destination ! someByteString
  (!) :: MonadIO io => h -> ByteString -> io ()

instance CanSend Handle where
  (!) = send

instance (Foldable f) => CanSend (f Handle) where
  (!) = broadcast

-- | Send a 'BSL.ByteString' message to exactly one 'I.Handle'.
send :: MonadIO io => Handle -> ByteString -> io ()
send h str = liftIO $ L.hPut h str' >> hFlush h
  where str' = runPut $ do
          putWord64le . fromIntegral $ L.length str
          putLazyByteString str

-- | Broadcast a 'BSL.ByteString' message to multiple 'I.Handle's.
broadcast :: MonadIO io => Foldable f => f Handle -> ByteString -> io ()
broadcast hs str = F.mapM_ (! str) hs

-- | Receive a 'BSL.ByteString' message from a 'I.Handle'.
receive :: MonadIO io => Handle -> io ByteString
receive h = liftIO $ do
  len <- fromIntegral . toInteger . runGet getWord64le <$> L.hGet h 8
  L.hGet h len
