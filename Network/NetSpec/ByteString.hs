{-# LANGUAGE FlexibleInstances #-}

module Network.NetSpec.ByteString (
    (!)
  , send
  , broadcast
  , receive
  , module I
  , module BSL
  ) where

import System.IO as I (Handle)
import Data.ByteString.Lazy as BSL (ByteString)

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import Data.Foldable as F
import System.IO (hFlush)
import Data.Binary.Get
import Data.Binary.Put


infix 2 !

class CanSend h where
  (!) :: h -> ByteString -> IO ()

instance CanSend Handle where
  (!) = send

instance (Foldable f) => CanSend (f Handle) where
  (!) = broadcast

send :: Handle -> ByteString -> IO ()
send h str = L.hPut h str' >> hFlush h
  where str' = runPut $ do
          putWord64le . fromIntegral $ L.length str
          putLazyByteString str

broadcast :: Foldable f => f Handle -> ByteString -> IO ()
broadcast hs str = F.mapM_ (! str) hs

receive :: Handle -> IO ByteString
receive h = do
  len <- fromIntegral . toInteger . runGet getWord64le <$> L.hGet h 8
  L.hGet h len
