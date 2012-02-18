{-# LANGUAGE TemplateHaskell #-}

module Network.NetSpec.Examples.BlackjackData where

import Network.NetSpec.Json


data Suit = Heart
          | Spade
          | Club 
          | Diamond
          deriving (Enum, Bounded)
$(deriveJson id ''Suit)

instance Show Suit where
  show Heart = "♥"
  show Spade = "♠"
  show Club = "♣"
  show Diamond = "♦"

data FaceValue = Number Int
               | King
               | Queen
               | Jack
               | Ace
$(deriveJson id ''FaceValue)

instance Show FaceValue where
  show (Number i) = show i
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show Ace = "A"

instance Enum FaceValue where
  toEnum 12 = Ace
  toEnum 11 = King
  toEnum 10 = Queen
  toEnum 9 = Jack
  toEnum (-1) = undefined
  toEnum n = Number (n + 2)

  fromEnum Ace = 12
  fromEnum King = 11
  fromEnum Queen = 10
  fromEnum Jack = 9
  fromEnum (Number n) = n - 2

instance Bounded FaceValue where
  minBound = Number 2
  maxBound = Ace


data Card = Card { suit :: Suit, faceValue :: FaceValue }
$(deriveJson id ''Card)

instance Show Card where
  show (Card s v) = "|" ++ show v ++ show s ++ "|"

type Hand = [Card]

data BlackjackClientMessage = Hit | Stand deriving (Show, Read)
$(deriveJson id ''BlackjackClientMessage)

data BlackjackState = BJ
  { hands :: [Hand]
  , deck :: [Card]
  }
$(deriveJson id ''BlackjackState)

data BJStartMsg = YouAre { yourIndex :: Int }
$(deriveJson id ''BJStartMsg)

data BJLoopMsg = YourTurn { yourHand :: Hand 
                          , opponentCard :: Card
                          , opponentNumCards :: Int
                          }
               | YouWin { endState :: BlackjackState }
               | YouLose { endState :: BlackjackState }
$(deriveJson id ''BJLoopMsg)

loFVal :: FaceValue -> Int
loFVal (Number n) = n
loFVal Ace = 1
loFVal _ = 10

hiFVal :: FaceValue -> Int
hiFVal (Number n) = n
hiFVal Ace = 11
hiFVal _ = 10

loVal :: Hand -> Int
loVal = sum . map (loFVal . faceValue)

hiVal :: Hand -> Int
hiVal = sum . map (hiFVal . faceValue)

-- TODO: handle case of 2 or more aces.
bestVal :: Hand -> Int
bestVal h = if hi > 21 then lo else hi
  where hi = hiVal h
        lo = loVal h

isBust :: Hand -> Bool
isBust h = loVal h > 21

isHit :: BlackjackClientMessage -> Bool
isHit Hit = True
isHit _ = False

isStand :: BlackjackClientMessage -> Bool
isStand Stand = True
isStand _ = False
