{-# LANGUAGE FlexibleInstances #-}

module Network.NetSpec (
    NetSpec (..)
  , SpecState (..)  
  , serve
  , (!)
  , send
  , broadcast
  , receive
  , continue
  , continue_
  , stop
  , stop_
  , module N
  , module I
  , module BS
  ) where

import qualified System.IO as I (Handle)
import qualified Network as N (PortID (..))
import qualified Data.ByteString.Char8 as BS (ByteString)

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 (ByteString)

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import System.IO
import Network
import Control.Applicative ((<$>))
import Control.Exception


fst' :: (a,b,c) -> a
fst' (a,_,_) = a


data SpecState s = Continue s | Stop s

continue :: Monad m => s -> m (SpecState s)
continue = return . Continue

continue_ :: Monad m => m (SpecState ())
continue_ = continue ()

stop :: Monad m => s -> m (SpecState s)
stop = return . Stop

stop_ :: Monad m => m (SpecState ())
stop_ = stop ()

instance Functor SpecState where
  fmap f (Continue s) = Continue $ f s
  fmap f (Stop s) = Stop $ f s

data NetSpec t s = NetSpec
  { _ports  :: t PortID
  , _begin  :: t Handle -> IO s
  , _loop   :: t Handle -> s -> IO (SpecState s)
  , _end    :: t Handle -> s -> IO ()
  }


infix 2 !

class CanSend h where
  (!) :: h -> ByteString -> IO ()

instance CanSend Handle where
  (!) = send

send :: Handle -> ByteString -> IO ()
send h str = C8.hPutStrLn h str >> hFlush h

instance (Foldable f) => CanSend (f Handle) where
  (!) = broadcast

broadcast :: Foldable f => f Handle -> ByteString -> IO ()
broadcast hs str = F.mapM_ (! str) hs

receive :: Handle -> IO ByteString
receive = C8.hGetLine


serve :: Traversable t => NetSpec t s -> IO ()
serve spec = withSocketsDo $ bracket a c b
  where
    a = do
      ss <- T.mapM listenOn $ _ports spec
      hs <- fmap fst' <$> T.mapM accept ss
      return (ss, hs)
    b (_, hs) = do
      s <- _begin spec hs
      runSpec hs s
    c (ss, hs) = do
      F.mapM_ hClose hs
      F.mapM_ sClose ss

    runSpec hs s = do
      res <- _loop spec hs s
      case res of
        Continue s' -> runSpec hs s'
        Stop s'     -> _end spec hs s'
