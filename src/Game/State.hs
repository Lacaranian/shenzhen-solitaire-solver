module Game.State (Game(..), Card(..), CardSuit(..), DragonSuit(..)) where

data Game = Game { freeCellCards :: [Maybe Card], goalCellCards :: [Maybe Card], cardStackCards :: [[Card]] } deriving (Eq, Show)

data Card = Card Int CardSuit | Dragon DragonSuit | Rose deriving (Eq, Show)
data CardSuit = Stones | Sticks | Characters deriving (Eq, Show)
data DragonSuit = Red | Green | White deriving (Eq, Show)


