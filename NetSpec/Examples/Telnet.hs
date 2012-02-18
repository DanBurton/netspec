-- no need for OverloadedStrings

module Network.NetSpec.Examples.Telnet where

import Network.NetSpec
import Network.NetSpec.Text
import System.Environment (getArgs)
import Data.Text as T
import Data.Text.IO as TIO
import Text.Printf (printf)
import Control.Monad (void)

readInteger :: String -> Integer
readInteger = read

-- printf freaks out if you don't use "void"
-- to help it figure out its correct type.
main :: IO ()
main = do
  [hostname, port] <- getArgs
  let p = PortNumber . fromInteger . readInteger $ port
  runSpec ClientSpec
    { _conns = [(hostname, p)]
    , _begin = \[h] -> do
         printf "Connected to %s on port %s\n" hostname port
         receive h >>= TIO.putStrLn
    , _loop = \[h] () -> do
         line <- TIO.getLine
         if T.null line
           then stop_
           else do h ! line
                   receive h >>= TIO.putStrLn
                   continue_
    , _end = \_ () -> printf "Disconnecting\n"
    }
