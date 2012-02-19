{-# LANGUAGE FlexibleInstances #-}

-- | Use 'X.Text' to send and receive messages,
-- delimited by @\n@
module NetSpec.Text (
    (!)
  , send
  , broadcast
  , receive
  , module I
  , module X
  , module IO
  ) where

import System.IO as I (Handle)
import Data.Text as X (Text, pack)
import Control.Monad.IO.Class as IO (MonadIO, liftIO)

import Data.Text.IO as T
import Data.Foldable as F
import System.IO (hFlush)


infix 2 !

class CanSend h where
  -- | The staple for sending a message.
  -- @!@ is typeclassed so that you can
  -- 'send' or 'broadcast' using the same simple syntax.
  -- 
  -- Usage:
  -- 
  -- > destination ! someText
  (!) :: MonadIO io => h -> Text -> io ()

instance CanSend Handle where
  (!) = send

instance (Foldable f) => CanSend (f Handle) where
  (!) = broadcast

-- | Send a 'X.Text' message to exactly one 'I.Handle'.
send :: MonadIO io => Handle -> Text -> io ()
send h str = liftIO $ T.hPutStrLn h str >> hFlush h

-- | Broadcast a 'X.Text' message to multiple 'I.Handle's.
broadcast :: MonadIO io => Foldable f => f Handle -> Text -> io ()
broadcast hs str = F.mapM_ (! str) hs

-- | Receive a 'X.Text' message from a 'I.Handle'.
receive :: MonadIO io => Handle -> io Text
receive = liftIO . T.hGetLine
