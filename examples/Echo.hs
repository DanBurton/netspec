{-# LANGUAGE OverloadedStrings #-}

import NetSpec
import NetSpec.Text

main :: IO ()
main = runSpec ServerSpec
  { _ports = [PortNumber 5001]
  , _begin = (! "I'll echo anything you say.")
  , _loop = \[h] () -> receive h >>= \line -> case line of
       "bye" -> stop_
       _     -> h ! line >> continue_
  , _end = \h () -> h ! "Bye bye now."
  }
