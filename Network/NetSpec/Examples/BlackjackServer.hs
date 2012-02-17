module Network.NetSpec.Examples.BlackjackServer where

import Network.NetSpec.Examples.BlackjackData

import Control.Applicative ((<$>))
import Control.Monad.State.Lazy
import Network.NetSpec
import Network.NetSpec.Json


shuffle :: [a] -> IO [a]
shuffle = return -- TODO

newGame :: BlackjackState
newGame = BJ [[],[]] fullDeck
  where fullDeck = [Card s v | s <- [minBound .. maxBound]
                             , v <- [minBound .. maxBound]]

stateT :: Monad m => (s -> (a, s)) -> StateT s m a
stateT = StateT . fmap return

deal :: Monad m => Int -> StateT BlackjackState m ()
deal 0 = stateT $ \(BJ [h1,h2] (c:cs)) -> ((), BJ [c:h1, h2] cs)
deal 1 = stateT $ \(BJ [h1,h2] (c:cs)) -> ((), BJ [h1, c:h2] cs)
deal _ = stateT $ \s -> ((), s)

bust :: BlackjackState -> Bool
bust _ = True -- TODO

stopIf :: Monad m => (s -> Bool) -> m s -> m (SpecState s)
stopIf f ms = do
  s <- ms
  if f s then stop s else continue s

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) f g x y = f (g x y)

isHit :: BlackjackClientMessage -> Bool
isHit Hit = True
isHit _ = False

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
  , _loop = \hs -> stopIf bust .: execStateT $
       forM_ (zip hs [0..]) $ \(h, n) -> do
         curHand <- ((!! n) . hands) <$> get
         h ! YourTurn curHand
         Just choice <- receive h
         when (isHit choice) (deal n)
  , _end = \[h0, h1] s -> do
       h0 ! YouWin s
       h1 ! YouLose s
  }

main :: IO ()
main = runSpec bjSpec
