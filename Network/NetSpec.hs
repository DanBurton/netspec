module Network.NetSpec (
    NetSpec (..)
  , SpecState (..)
  
  , runSpec
  , continue
  , continue_
  , continueIf
  , continueIf'
  , stop
  , stop_
  , stopIf
  , stopIf'
  
  , (.:)
  , stateT
  , module I
  , module N
  , module S
  , module A
  ) where

import System.IO as I (Handle)
import Network as N (PortID (..))
import Control.Monad.State as S
  (StateT (..), execStateT, evalStateT, get, put)
import Control.Applicative as A ((<$>))

import Control.Monad
import Control.Exception
import Data.Traversable as T
import Data.Foldable as F
import Network
import System.IO (hClose)

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

stateT :: Monad m => (s -> (a, s)) -> StateT s m a
stateT = StateT . fmap return

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) f g x y = f (g x y)


data SpecState s = Continue s | Stop s

continue :: Monad m => s -> m (SpecState s)
continue = return . Continue

continue_ :: Monad m => m (SpecState ())
continue_ = continue ()

stop :: Monad m => s -> m (SpecState s)
stop = return . Stop

stop_ :: Monad m => m (SpecState ())
stop_ = stop ()

continueIf :: Monad m => (a -> s -> Bool) -> m (a,s) -> m (SpecState s)
continueIf f ms = do
  (a,s) <- ms
  if f a s then continue s else stop s

continueIf' :: Monad m => (s -> Bool) -> m s -> m (SpecState s)
continueIf' f ms = continueIf (\() s -> f s) (liftM ((,) ()) ms)

stopIf :: Monad m => (a -> s -> Bool) -> m (a,s) -> m (SpecState s)
stopIf f ms = do
  (a,s) <- ms
  if f a s then stop s else continue s

stopIf' :: Monad m => (s -> Bool) -> m s -> m (SpecState s)
stopIf' f ms = stopIf (\() s -> f s) (liftM ((,) ()) ms)

instance Functor SpecState where
  fmap f (Continue s) = Continue $ f s
  fmap f (Stop s) = Stop $ f s


data NetSpec t s = ServerSpec
  { _ports  :: t PortID
  , _begin  :: t Handle -> IO s
  , _loop   :: t Handle -> s -> IO (SpecState s)
  , _end    :: t Handle -> s -> IO ()
  }
                 | ClientSpec
  { _conns  :: t (String, PortID)
  , _begin  :: t Handle -> IO s
  , _loop   :: t Handle -> s -> IO (SpecState s)
  , _end    :: t Handle -> s -> IO ()
  }


runSpec :: Traversable t => NetSpec t s -> IO ()
runSpec spec = withSocketsDo $ case spec of
    ServerSpec{} -> bracket a c b
    ClientSpec{} -> bracket a' c' b'
  where
    a = do
      ss <- T.mapM listenOn $ _ports spec
      hs <- fmap fst' <$> T.mapM accept ss
      return (ss, hs)
    b (_, hs) = _begin spec hs >>= go hs
    c (ss, hs) = do
      F.mapM_ hClose hs
      F.mapM_ sClose ss

    a' = T.mapM (uncurry connectTo) $ _conns spec
    b' hs = _begin spec hs >>= go hs
    c' = F.mapM_ hClose

    go hs s = do
      res <- _loop spec hs s
      case res of
        Continue s' -> go hs s'
        Stop s'     -> _end spec hs s'
