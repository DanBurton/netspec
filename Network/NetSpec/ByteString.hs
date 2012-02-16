{-# LANGUAGE FlexibleInstances #-}

module Network.NetSpec.ByteString (
    (!)
  , send
  , broadcast
  , receive
  , module I
  , module BS
  ) where

import System.IO as I (Handle)
import Data.ByteString.Char8 as BS (ByteString)

import Data.ByteString.Char8 as C8
import Data.Foldable as F
import System.IO (hFlush)


infix 2 !

class CanSend h where
  (!) :: h -> ByteString -> IO ()

instance CanSend Handle where
  (!) = send

instance (Foldable f) => CanSend (f Handle) where
  (!) = broadcast

send :: Handle -> ByteString -> IO ()
send h str = C8.hPutStrLn h str >> hFlush h

broadcast :: Foldable f => f Handle -> ByteString -> IO ()
broadcast hs str = F.mapM_ (! str) hs

receive :: Handle -> IO ByteString
receive = C8.hGetLine
