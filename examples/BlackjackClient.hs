import BlackjackData

import NetSpec
import NetSpec.Json

import Control.Monad (void)
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Printf (printf)

readInt :: String -> Int
readInt = read

data ClientState = CS
  { myIndex :: Int }

askAction :: Hand -> IO BlackjackClientMessage
askAction _ = putStrLn "Hit or Stand?" >> read <$> getLine

botAction :: Hand -> IO BlackjackClientMessage
botAction h
  | hiVal h > 17 = putStrLn "Stand" >> return Stand
  | loVal h > 14 = putStrLn "Stand" >> return Stand
  | otherwise    = putStrLn "Hit" >> return Hit

resolution :: String -> ClientState -> BlackjackState -> IO ()
resolution str (CS i) (BJ hs _) = printEnd hs i >> putStrLn str

win :: ClientState -> BlackjackState -> IO ()
win = resolution "You win!"

lose :: ClientState -> BlackjackState -> IO ()
lose = resolution "You lose!"

printEnd :: [Hand] -> Int -> IO ()
printEnd [h0,h1] i = let (you, opp) = if i == 0 then (h0,h1) else (h1,h0)
                     in do putStrLn $ replicate 40 '-'
                           void $ printf "You:  %s (%d)\n" (show you) (bestVal you)
                           void $ printf "Them: %s (%d)\n" (show opp) (bestVal opp)
printEnd _ _ = error "Unexpected number of hands"

displayTurn :: Hand -> Card -> Int -> IO ()
displayTurn hand c n = do
  void $ printf "You: %s" (show hand)
  putStrLn $ replicate 20 ' '
  void $ printf "Opponent: [%s,%s]\n" (show c) oHand
  where oHand = intercalate "," $ replicate (pred n) "|??|"

main :: IO ()
main = do
  (host:port:bot) <- getArgs
  let chooseAction = if null bot then askAction else botAction
  runSpec ClientSpec
    { _conns = [(host, PortNumber (fromIntegral $ readInt port))]
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
