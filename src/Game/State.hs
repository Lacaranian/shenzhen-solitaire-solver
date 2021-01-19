module Game.State (Game(..), Card(..), CardSuit(..), DragonSuit(..), GamePosition(..)) where

import Control.Monad (join)
import Util ((!!?))

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


data GamePosition = FreeCellSlot Int 
                  | GoalCellSlot Int
                  | CardStackSlot Int Int -- 0 indexed from an empty stack

data CardSlotContents = OneCard Card | NoCard

cardAtPosition :: Game -> GamePosition -> Maybe Card
cardAtPosition game (FreeCellSlot idx)          = join $ freeCellCards game !!? idx
cardAtPosition game (GoalCellSlot idx)          = join $ goalCellCards game !!? idx
cardAtPosition game (CardStackSlot stackID idx) = do 
    stack <- cardStackCards game !!? stackID
    stack !!? idx