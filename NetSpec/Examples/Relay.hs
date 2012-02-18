{-# LANGUAGE OverloadedStrings #-}

module NetSpec.Examples.Relay where

import NetSpec
import NetSpec.Text

main :: IO ()
main = runSpec ServerSpec {
    _ports = [PortNumber 5001, PortNumber 5002]
  , _begin = \[inHandle, outHandle] -> do
       inHandle ! "Begin sending your message. Say \"bye\" to quit."
       outHandle ! "Prepare to receive messages."
  , _loop = \[inHandle, outHandle] () -> do
       line <- receive inHandle
       case line of
         "bye\r" -> stop_
         _       -> outHandle ! line >> continue_
  , _end = \hs () -> hs ! "That's all folks."
  }
