{-# LANGUAGE OverloadedStrings #-}

module Network.NetSpec.Examples.Echo where

import Network.NetSpec

main :: IO ()
main = serve NetSpec {
    _ports = [PortNumber 5001, PortNumber 5002]
  , _begin = \[inHandle, outHandle] -> do
       inHandle ! "Begin sending your message. Say \"bye\" to quit."
       outHandle ! "Prepare to receive messages."
  , _loop = \[inHandle, outHandle] () -> do
       line <- receive inHandle
       case line of
         "bye\r" -> return $ End ()
         _       -> do
           outHandle ! line
           return $ Continue ()
  , _end = \hs () -> do
       broadcast hs "That's all folks."
  , _debug = debugPrint
  }
