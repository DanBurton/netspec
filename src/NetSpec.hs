-- | Simplify static Networking tasks.
module NetSpec (
  -- * Types and Constructors
    NetSpec (..)
  , SpecState (..)
  
  -- * Functions
  
  -- ** Running a NetSpec
  , runSpec
  
  -- ** Continue and Stop Combinators
  , continue
  , continue_
  , continueIf
  , continueIf'
  , stop
  , stop_
  , stopIf
  , stopIf'
  
  -- * Convenience
  
  -- ** Composition
  , (.:)

  -- ** IO and Networking
  , module I
  , module N
  
  -- ** Functors
  , module A
  
  -- ** State
  , module S
  , stateT
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

-- | Lift a state function into a 'S.StateT' monad stack
stateT :: Monad m => (s -> (a, s)) -> StateT s m a
stateT = StateT . fmap return

-- | Compose two functions, similar to @.@ from "Prelude".
-- If @h = f .: g@ then @h x y = f (g x y)@.
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) f g x y = f (g x y)

-- | Indicate whether to @Continue@ or @Stop@
-- with a given state
data SpecState s = Continue s | Stop s

-- | Continue with a given state
continue :: Monad m => s -> m (SpecState s)
continue = return . Continue

-- | Continue (statless)
continue_ :: Monad m => m (SpecState ())
continue_ = continue ()

-- | Stop with a given state
stop :: Monad m => s -> m (SpecState s)
stop = return . Stop

-- | Stop (stateless)
stop_ :: Monad m => m (SpecState ())
stop_ = stop ()

-- | Conditionally continue with a given state,
-- based on that state and additional given information.
-- 
-- Recommended usage:
-- 
-- > _loop = \handles -> continueIf f .: runStateT $ do ...
continueIf :: Monad m => (a -> s -> Bool) -> m (a,s) -> m (SpecState s)
continueIf f ms = do
  (a,s) <- ms
  if f a s then continue s else stop s

-- | Conditionally continue statelessly,
-- based on given information.
-- 
-- Recommended usage
-- 
-- > _loop = \handles () -> continueIf_ f $ do ...
continueIf_ :: Monad m => (a -> Bool) -> m a -> m (SpecState ())
continueIf_ f ms = continueIf (\a () -> f a) (liftM (\x -> (x,())) ms)

-- | Conditionally continue with a given state,
-- based solely on that state.
-- 
-- Recommended usage:
-- 
-- > _loop = \handles -> continueIf' f .: execStateT $ do ...
continueIf' :: Monad m => (s -> Bool) -> m s -> m (SpecState s)
continueIf' f ms = continueIf (\() s -> f s) (liftM ((,) ()) ms)

-- | Conditionally stop with a given state,
-- based on that state and additional given information.
stopIf :: Monad m => (a -> s -> Bool) -> m (a,s) -> m (SpecState s)
stopIf f = continueIf (not . f)

-- | Conditionally stop with a given state,
-- based solely on that state.
stopIf' :: Monad m => (s -> Bool) -> m s -> m (SpecState s)
stopIf' f = continueIf' (not . f)

-- | Conditionally stop statlessly,
-- based on given information.
stopIf_ :: Monad m => (s -> Bool) -> m a -> m (SpecState ())
stopIf_ f = continueIf_ (not . f)

instance Functor SpecState where
  fmap f (Continue s) = Continue $ f s
  fmap f (Stop s) = Stop $ f s

-- | Define the specification of your networking task
-- as a begin, loop, and end proceedure. Run your NetSpec
-- with 'runSpec'.
-- 
-- @t@ indicates the 'T.Traversable' structure used
-- (@[]@ is recommended for simplicity, but you are at liberty
-- to use any Traversable you see fit.
-- 
-- @s@ indicates the type used for "state".
-- Use @()@ for a stateless specification.
data NetSpec t s
  -- | A server must specify which ports to listen on
  = ServerSpec
  { _ports  :: t PortID
  , _begin  :: t Handle -> IO s
  , _loop   :: t Handle -> s -> IO (SpecState s)
  , _end    :: t Handle -> s -> IO ()
  }
  
  -- | A client must specify tuples of (hostname, port) to connect to.
  | ClientSpec
  { _conns  :: t (String, PortID)
  , _begin  :: t Handle -> IO s
  , _loop   :: t Handle -> s -> IO (SpecState s)
  , _end    :: t Handle -> s -> IO ()
  }


-- | Run a 'NetSpec'.
-- 
-- Running a spec will step through your 'T.Traversable'
-- of connections, and replace each one with a 'I.Handle',
-- preserving the structure of the Traversable otherwise.
-- 
-- Regardless of exceptions, all Handles and Sockets
-- opened by the spec will be closed at the end of the run;
-- you should not need to close any of the Handles given to you
-- by the spec.
-- 
-- During the run, it is assumed that all connections will
-- remain open at least until they are no longer needed.
-- (Note that this calls 'I.withSocketsDo' for you)
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
