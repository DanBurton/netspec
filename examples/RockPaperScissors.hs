{-# LANGUAGE TemplateHaskell #-}


import System.Environment (getArgs)
import System.Random
import Control.Arrow (first)
import Data.Maybe (fromJust)

import Network.NetSpec
import Network.NetSpec.Json

data Choice = Rock | Paper | Scissors
            deriving (Eq, Read, Show, Enum, Bounded)
$(deriveJson id ''Choice)

instance Random Choice where
  randomR (lo,hi) = first toEnum . randomR (fromEnum lo, fromEnum hi)
  random = randomR (minBound, maxBound)

vs :: Choice -> Choice -> Ordering
Rock `vs` Scissors   = GT
Scissors `vs` Paper  = GT
Paper `vs` Rock      = GT
x `vs` y | x == y    = EQ
         | otherwise = LT

data Result = YouWin | YouLose | Tie
$(deriveJson id ''Result)

data Round = Again | NoMore deriving (Eq, Read)
$(deriveJson id ''Round)

data Score = Score { wins, ties, losses :: !Int }
           deriving Show
$(deriveJson id ''Score)

flipScore :: Score -> Score
flipScore (Score w t l) = Score l t w

newScore :: Score
newScore = Score 0 0 0

strToPort :: String -> PortID
strToPort = PortNumber . fromIntegral . read

h <!> m = do j <- m;
             h ! j

main :: IO ()
main = do
  args <- getArgs
  case args of
    "client":xs -> runClient xs
    "server":xs -> runServer xs
    _ -> error "First arg must be 'server' or 'client'"


win, lose, tie :: Monad m => StateT Score m (Result, Result)
win  = stateT $ \(Score w t l) -> ((YouWin, YouLose), Score (w+1) t l)
lose = stateT $ \(Score w t l) -> ((YouLose, YouWin), Score w t (l+1))
tie  = stateT $ \(Score w t l) -> ((Tie, Tie),        Score w (t+1) l)

printScore :: Score -> IO ()
printScore = print                  


runServer :: [String] -> IO ()
runServer [port1,port2] = runSpec ServerSpec
  { _ports = [ strToPort port1
             , strToPort port2
             ]
  , _begin = \_ -> return newScore
  , _loop  = \ps@[p1,p2] -> continueIf allAgain .: runStateT $ do
       ps ! Again
       [Just c1, Just c2] <- mapM receive ps
       (m1, m2) <- case c1 `vs` c2 of
             GT -> win -- (YouWin, YouLose)
             EQ -> tie -- (Tie, Tie)
             LT -> lose -- (YouLose, YouWin)
       p1 ! m1
       p2 ! m2
       mapM (liftIO . askNextRound) ps
  , _end = \ps@[p1,p2] s -> do
       ps ! NoMore
       printScore s
       p1 ! s
       p2 ! flipScore s
  }
  where
    askNextRound :: Handle -> IO Round
    askNextRound p = fromJust <$> receive p
    
    allAgain :: [Round] -> Score -> Bool
    allAgain = const . all (== Again)

runServer _ = error "Usage: RockPaperScissors server PORT1 PORT2"


runClient :: [String] -> IO ()
runClient (host:port:bot) = runSpec ClientSpec
  { _conns = [(host, strToPort port)]
  , _begin = \_ -> putBegin
  , _loop = \[h] () -> do
       Just m <- receive h
       case m of
         Again -> do
           h <!> getMove
           Just r <- receive h
           putResult r
           again <- getAgain
           h ! again
           case again of
             Again  -> continue_
             NoMore -> receive h >>= (\(Just NoMore) -> stop_)
         NoMore -> putServerEnd >> stop_
  , _end = \[h] () -> do
       Just s <- receive h
       printScore s
       putEnd
  }
  where
    putBegin = putStrLn "Let's play some Rock Paper Scissors!"
    putResult r = putStrLn $ case r of
      YouWin -> "You win!"
      YouLose -> "You lose!"
      Tie -> "Tie!"
    putServerEnd = putStrLn "Sorry, the server says no more."
    putEnd = putStrLn "See you later."

    getMove :: IO Choice
    getMove = if null bot then getMovePerson else getMoveBot
    getMovePerson = putStrLn "Rock, Paper, or Scissors?" >> fmap read getLine
    getMoveBot = do x <- randomIO; print x; return x

    getAgain :: IO Round
    getAgain = putStrLn "Again or NoMore?" >> fmap read getLine

runClient _ = error "Usage: RockPaperScissors client HOSTNAME PORT [bot]"
