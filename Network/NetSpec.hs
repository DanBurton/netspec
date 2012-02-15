module Network.NetSpec (
    NetSpec (..)
  , SpecState (..)  
  , serve
  , (!)
  , broadcast
  , receive
  , debugPrint
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

import System.IO
import Network
import Control.Applicative ((<$>))
import Control.Exception
import Data.Maybe (fromMaybe)


fst' :: (a,b,c) -> a
fst' (a,_,_) = a


data SpecState s = Continue s | End s

instance Functor SpecState where
  fmap f (Continue s) = Continue $ f s
  fmap f (End s) = End $ f s

data NetSpec t s = NetSpec
  { _ports  :: t PortID
  , _begin  :: t Handle -> IO s
  , _loop   :: t Handle -> s -> IO (SpecState s)
  , _end    :: t Handle -> s -> IO ()
  , _debug  :: Maybe (String -> IO ())
  }


(!) :: Handle -> ByteString -> IO ()
h ! str = C8.hPutStrLn h str >> hFlush h

broadcast :: Traversable t => t Handle -> ByteString -> IO ()
broadcast hs str = F.mapM_ (! str) hs

receive :: Handle -> IO ByteString
receive = C8.hGetLine

debugPrint :: Maybe (String -> IO ())
debugPrint = Just (hPutStrLn stderr)


serve :: Traversable t => NetSpec t s -> IO ()
serve spec = withSocketsDo $ bracket a c b
  where
    a = do
      d $ "Listening on ports: " ++
        (unwords . F.toList $ fmap (\(PortNumber n) -> show n) (_ports spec))
      ss <- T.mapM listenOn $ _ports spec
      d "Accepting on socks"
      hs <- fmap fst' <$> T.mapM accept ss
      return (ss, hs)
    b (_, hs) = do
      d "Ready to begin"
      s <- _begin spec hs
      d "Begun"
      runSpec hs s
      d "Ended"
    c (ss, hs) = do
      F.mapM_ hClose hs
      d "Handles closed"
      F.mapM_ sClose ss
      d "Sockets closed"

    runSpec hs s = do
      putStrLn "Ready to loop"
      res <- _loop spec hs s
      putStrLn "Looped"
      case res of
        Continue s' -> runSpec hs s'
        End s'      -> _end spec hs s'
    
    d = fromMaybe (\_ -> return ()) (_debug spec)
