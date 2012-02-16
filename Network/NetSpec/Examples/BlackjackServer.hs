module Network.NetSpec.Examples.BlackjackServer where

import Network.NetSpec.Examples.BlackjackData

import Control.Applicative ((<$>))
import Control.Monad.State.Lazy
import Network.NetSpec
import Network.NetSpec.Json


shuffle :: [a] -> IO [a]
shuffle = return -- TODO

newGame :: IO BlackjackState
newGame = BJ [[],[]] <$> shuffle fullDeck
  where fullDeck = [Card s v | s <- [minBound .. maxBound]
                             , v <- [minBound .. maxBound]]

deal :: Int -> State BlackjackState ()
deal 0 = state $ \(BJ [h1,h2] (c:cs)) -> ((), BJ [c:h1, h2] cs)
deal 1 = state $ \(BJ [h1,h2] (c:cs)) -> ((), BJ [h1, c:h2] cs)
deal _ = state $ \s -> ((), s)

bust :: BlackjackState -> Bool
bust _ = True -- TODO


bjSpec :: NetSpec [] BlackjackState
bjSpec = ServerSpec {
    _ports = map PortNumber [5001, 5002]
  , _begin = \[h0, h1] -> do
       h0 ! YouAre 0
       h1 ! YouAre 1
       s <- newGame
       let s' = flip execState s $ replicateM_ 2 (deal 0 >> deal 1)
       return s'
  , _loop = \[h0,h1] s -> do
       h0 ! YourTurn (hands s !! 0)
       h1 ! YourTurn (hands s !! 1)
       Just choice0 <- receive h0
       Just choice1 <- receive h1
       let s' = case choice0 of
             Hit -> execState (deal 0) s
             Stand -> s
           s'' = case choice1 of
             Hit -> execState (deal 1) s'
             Stand -> s'
       if bust s'' then stop s'' else continue s''
  , _end = \[h0, h1] s -> do
       h0 ! YouWin s
       h1 ! YouLose s
  }

main :: IO ()
main = runSpec bjSpec
