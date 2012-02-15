module Network.NetSpec.Examples.Echo where

import Network.NetSpec

main :: IO ()
main = serve NetSpec {
    _ports = [PortNumber 5001, PortNumber 5002]
  , _begin = \hs@[inHandle, outHandle] -> do
       send inHandle "Begin sending your message"
       send outHandle "Prepare to receive messages"
       flush hs
  , _loop = \() hs@[inHandle, outHandle] -> do
       line <- recv inHandle
       case line of
         "bye\r" -> return $ End ()
         _       -> do
           send outHandle line
           flush hs
           return (Continue ())
  , _end = \() hs@[inHandle, outHandle] -> do
       send inHandle "Thanks for sending"
       send outHandle "Thanks for receiving; that's all"
       flush hs
  , _debug = debugPrint
  }

