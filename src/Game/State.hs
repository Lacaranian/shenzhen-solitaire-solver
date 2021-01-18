module Game.State (Game(..), Card(..), CardSuit(..), DragonSuit(..)) where

data Game = Game { freeCellCards :: [Maybe Card], goalCellCards :: [Maybe Card], cardStackCards :: [[Card]] } deriving (Eq, Show)

data Card = Card Int CardSuit 
          | Dragon DragonSuit 
          | Rose 
          deriving (Eq, Show)

data CardSuit = Stones     -- Red
              | Sticks     -- Green
              | Characters -- Black
              deriving (Eq, Show)

data DragonSuit = Red   -- Arrow
                | Green -- Character
                | White -- Square
                deriving (Eq, Show)


