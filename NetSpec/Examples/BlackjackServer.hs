module NetSpec.Examples.BlackjackServer where

import NetSpec.Examples.BlackjackData

import NetSpec
import NetSpec.Json
import Control.Monad

-- cabal install random-shuffle
import System.Random.Shuffle (shuffleM)

shuffle :: [a] -> IO [a]
shuffle = shuffleM -- TODO: not depend on random-shuffle

newGame :: BlackjackState
newGame = BJ [[],[]] fullDeck
  where fullDeck = [Card s v | s <- [minBound .. maxBound]
                             , v <- [minBound .. maxBound]]

deal :: Monad m => Int -> StateT BlackjackState m ()
deal 0 = stateT $ \(BJ [h1,h2] (c:cs)) -> ((), BJ [c:h1, h2] cs)
deal 1 = stateT $ \(BJ [h1,h2] (c:cs)) -> ((), BJ [h1, c:h2] cs)
deal _ = stateT $ \s -> ((), s)

bustOrStands :: [BlackjackClientMessage] -> BlackjackState -> Bool
bustOrStands cs s = any isBust (hands s) || all isStand cs

whoWins :: BlackjackState -> Handle -> Handle -> (Handle, Handle)
whoWins (BJ [h0, h1] _) p0 p1
  | isBust h1               = (p0, p1)
  | isBust h0               = (p1, p0)
  | bestVal h1 > bestVal h0 = (p1, p0)
  | otherwise               = (p0, p1)
whoWins _ _ _ = error "Unexpected BlackjackState format"

bjSpec :: NetSpec [] BlackjackState
bjSpec = ServerSpec {
    _ports = map PortNumber [5001, 5002]
  , _begin = \[h0, h1] -> flip execStateT newGame $ do
       h0 ! YouAre 0
       h1 ! YouAre 1
       s <- get
       deck' <- liftIO $ shuffle $ deck s
       put $ s { deck = deck' }
       replicateM_ 2 (deal 0 >> deal 1)
  , _loop = \hs -> stopIf bustOrStands .: runStateT $
       forM (zip hs [0..]) $ \(h, n) -> do
         curHand <- ((!! n) . hands) <$> get
         opponentHand <- ((!! rem (n+1) 2) . hands) <$> get
         h ! YourTurn curHand (last opponentHand) (length opponentHand)
         Just choice <- receive h
         when (isHit choice) (deal n)
         return choice
  , _end = \[h0, h1] s -> do
       let (winner, loser) = whoWins s h0 h1
       winner ! YouWin s
       loser ! YouLose s
  }

main :: IO ()
main = runSpec bjSpec
