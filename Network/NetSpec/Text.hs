{-# LANGUAGE FlexibleInstances #-}

module Network.NetSpec.Text (
    (!)
  , send
  , broadcast
  , receive
  , module I
  , module X
  ) where

import System.IO as I (Handle)
import Data.Text as X (Text)

import Data.Text.IO as T
import Data.Foldable as F
import System.IO (hFlush)


infix 2 !

class CanSend h where
  (!) :: h -> Text -> IO ()

instance CanSend Handle where
  (!) = send

instance (Foldable f) => CanSend (f Handle) where
  (!) = broadcast

send :: Handle -> Text -> IO ()
send h str = T.hPutStrLn h str >> hFlush h

broadcast :: Foldable f => f Handle -> Text -> IO ()
broadcast hs str = F.mapM_ (! str) hs

receive :: Handle -> IO Text
receive = T.hGetLine
