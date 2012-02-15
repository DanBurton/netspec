module Network.NetSpec (
    NetSpec (..)
  , SpecState (..)  
  , serve
  , send
  , broadcast
  , recv
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

data NetSpec s = NetSpec
  { _ports  :: [PortID]
  , _begin  :: [Handle] -> IO s
  , _loop   :: s -> [Handle] -> IO (SpecState s)
  , _end    :: s -> [Handle] -> IO ()
  , _debug  :: Maybe (String -> IO ())
  }


send, (!) :: Handle -> ByteString -> IO ()
send h str = C8.hPutStrLn h str >> hFlush h

(!) = send

broadcast :: [Handle] -> ByteString -> IO ()
broadcast hs str = mapM_ (! str) hs

recv :: Handle -> IO ByteString
recv = C8.hGetLine

debugPrint :: Maybe (String -> IO ())
debugPrint = Just (hPutStrLn stderr)


serve :: NetSpec s -> IO ()
serve spec = withSocketsDo $ bracket a c b
  where
    a = do
      d $ "Listening on ports: " ++
        unwords (map (\(PortNumber n) -> show n) (_ports spec))
      ss <- mapM listenOn $ _ports spec
      d "Accepting on socks"
      hs <- map fst' <$> mapM accept ss
      return (ss, hs)
    b (_, hs) = do
      d "Ready to begin"
      s <- _begin spec hs
      d "Begun"
      runSpec s hs
      d "Ended"
    c (ss, hs) = do
      mapM_ hClose hs
      d "Handles closed"
      mapM_ sClose ss
      d "Sockets closed"

    runSpec s hs = do
      putStrLn "Ready to loop"
      res <- _loop spec s hs
      putStrLn "Looped"
      case res of
        Continue s' -> runSpec s' hs
        End s'      -> _end spec s' hs
    
    d = fromMaybe (\_ -> return ()) (_debug spec)
