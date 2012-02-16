module Network.NetSpec.Examples.BlackjackClient where

import Network.NetSpec.Examples.BlackjackData
import Network.NetSpec
import Network.NetSpec.Json
import System.Environment (getArgs)
import Control.Applicative ((<$>))

readInt :: String -> Int
readInt = read

data ClientState = CS
  { myIndex :: Int }

chooseAction :: Hand -> IO BlackjackClientMessage
chooseAction h = print h >> return Hit -- TODO

win :: ClientState -> BlackjackState -> IO ()
win _ _ = putStrLn "You win" >> return () --TODO

lose :: ClientState -> BlackjackState -> IO ()
lose _ _ = putStrLn "You lose" >> return () --TODO

main :: IO ()
main = do
  [port] <- map (fromIntegral . readInt) <$> getArgs
  runSpec ClientSpec
    { _conns = [("localhost", PortNumber port)]
    , _begin = \[h] -> do
         Just m <- receive h
         case m of
           YouAre i -> return $ CS i
           _ -> error "Unexpected server response"
    , _loop = \[h] s -> do
         Just m <- receive h
         case m of
           YourTurn hand -> do
             action <- chooseAction hand
             h ! action
             continue s
           YouWin gs -> win s gs >> stop s
           YouLose gs -> lose s gs >> stop s
           _ -> error "Unexpected server response"
    , _end = \_ _ -> return ()
    }
