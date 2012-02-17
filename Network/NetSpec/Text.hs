{-# LANGUAGE FlexibleInstances #-}

module Network.NetSpec.Text (
    (!)
  , send
  , broadcast
  , receive
  , module I
  , module X
  , module IO
  ) where

import System.IO as I (Handle)
import Data.Text as X (Text)
import Control.Monad.IO.Class as IO (MonadIO, liftIO)

import Data.Text.IO as T
import Data.Foldable as F
import System.IO (hFlush)


infix 2 !

class CanSend h where
  (!) :: MonadIO io => h -> Text -> io ()

instance CanSend Handle where
  (!) = send

instance (Foldable f) => CanSend (f Handle) where
  (!) = broadcast

send :: MonadIO io => Handle -> Text -> io ()
send h str = liftIO $ T.hPutStrLn h str >> hFlush h

broadcast :: MonadIO io => Foldable f => f Handle -> Text -> io ()
broadcast hs str = F.mapM_ (! str) hs

receive :: MonadIO io => Handle -> io Text
receive = liftIO . T.hGetLine
