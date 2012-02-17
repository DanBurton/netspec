module Network.NetSpec.Examples.BlackjackClient where

import Network.NetSpec.Examples.BlackjackData
import Network.NetSpec
import Network.NetSpec.Json
import System.Environment (getArgs)
import Data.List (intercalate)
import Text.Printf (printf)

readInt :: String -> Int
readInt = read

data ClientState = CS
  { myIndex :: Int }

askAction :: Hand -> IO BlackjackClientMessage
askAction h = putStrLn "Hit or Stand?" >> read <$> getLine

botAction :: Hand -> IO BlackjackClientMessage
botAction h
  | hiVal h > 17 = putStrLn "Stand" >> return Stand
  | loVal h > 14 = putStrLn "Stand" >> return Stand
  | otherwise    = putStrLn "Hit" >> return Hit

win :: ClientState -> BlackjackState -> IO ()
win (CS i) (BJ hs _) = printEnd hs i >> putStrLn "You win"

lose :: ClientState -> BlackjackState -> IO ()
lose (CS i) (BJ hs _) = printEnd hs i >> putStrLn "You lose"

printEnd :: [Hand] -> Int -> IO ()
printEnd [h0,h1] i = let (you, opp) = if i == 0 then (h0,h1) else (h1,h0)
                     in do putStrLn $ replicate 40 '-'
                           printf "You:  %s (%d)\n" (show you) (bestVal you)
                           printf "Them: %s (%d)\n" (show opp) (bestVal opp)

displayTurn :: Hand -> Card -> Int -> IO ()
displayTurn hand c n = do
  printf "You: %s" (show hand)
  putStrLn $ replicate 20 ' '
  printf "Opponent: [%s,%s]\n" (show c) oHand
  where oHand = intercalate "," $ replicate (pred n) "|??|"

main :: IO ()
main = do
  (port:bot) <- map (fromIntegral . readInt) <$> getArgs
  let chooseAction = if null bot then askAction else botAction
  runSpec ClientSpec
    { _conns = [("localhost", PortNumber port)]
    , _begin = \[h] -> do
         Just m <- receive h
         case m of YouAre i -> return $ CS i
    , _loop = \[h] s -> do
         Just m <- receive h
         case m of
           YourTurn hand c n -> do
             displayTurn hand c n
             action <- chooseAction hand
             h ! action
             continue s
           YouWin gs -> win s gs >> stop s
           YouLose gs -> lose s gs >> stop s
    , _end = \_ _ -> return ()
    }
