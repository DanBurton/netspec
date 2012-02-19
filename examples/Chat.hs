{-# LANGUAGE OverloadedStrings #-}

import Network.NetSpec
import Network.NetSpec.Text

-- Notice the restrictions NetSpec places on how you can communicate.
-- Working within NetSpec isn't well-suited for all situations;
--  it is specialized for situations where there is
--  a deterministic sequence of communication.

main :: IO ()
main = runSpec ServerSpec {
    _ports = [PortNumber 5001, PortNumber 5002]
  , _begin = \hs -> do
       hs ! "Welcome to the one-for-one chat program."
       hs ! "Send a message in order to receive one."
       hs ! "Say \"bye\" to stop."
  , _loop = \[a, b] () -> do
       lineA <- receive a
       lineB <- receive b
       a ! lineB
       b ! lineA
       if lineA == "bye\r" || lineB == "bye\r"
         then stop_ else continue_
  , _end = \hs () -> hs ! "That's all folks."
  }
