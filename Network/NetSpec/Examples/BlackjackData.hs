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

data BlackjackClientMessage = Hit | Stand
$(deriveJson id ''BlackjackClientMessage)

data BlackjackState = BJ
  { hands :: [Hand]
  , deck :: [Card]
  }
$(deriveJson id ''BlackjackState)

data BlackjackServerMessage = YouAre { yourIndex :: Int }
                            | YourTurn { yourHand :: Hand }
                            | YouWin { endState :: BlackjackState }
                            | YouLose { gameState :: BlackjackState }
                            | Error
$(deriveJson id ''BlackjackServerMessage)
