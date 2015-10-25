{-# LANGUAGE TemplateHaskell #-}

-- This test makes sure that the Blackjack example
-- compiles and quickly runs to completion with 2 bots.
module Main where

import qualified BlackjackServer
import qualified BlackjackClient
import Control.Concurrent.Async (async, wait)
import System.Environment (withArgs)
import System.Exit (exitFailure)
import System.Timeout (timeout)

main :: IO ()
main = do
  server <- async $ BlackjackServer.main
  client1 <- async $ withArgs (words "localhost 5001 bot") $ BlackjackClient.main
  client2 <- async $ withArgs (words "localhost 5002 bot") $ BlackjackClient.main
  mt <- timeout 1000000 $ mapM_ wait [server, client1, client2]
  case mt of
    Just{} -> return ()
    Nothing -> exitFailure
