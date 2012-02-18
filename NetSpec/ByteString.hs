{-# LANGUAGE FlexibleInstances #-}

module Network.NetSpec.ByteString (
    (!)
  , send
  , broadcast
  , receive
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
  (!) :: MonadIO io => h -> ByteString -> io ()

instance CanSend Handle where
  (!) = send

instance (Foldable f) => CanSend (f Handle) where
  (!) = broadcast

send :: MonadIO io => Handle -> ByteString -> io ()
send h str = liftIO $ L.hPut h str' >> hFlush h
  where str' = runPut $ do
          putWord64le . fromIntegral $ L.length str
          putLazyByteString str

broadcast :: MonadIO io => Foldable f => f Handle -> ByteString -> io ()
broadcast hs str = F.mapM_ (! str) hs

receive :: MonadIO io => Handle -> io ByteString
receive h = liftIO $ do
  len <- fromIntegral . toInteger . runGet getWord64le <$> L.hGet h 8
  L.hGet h len
