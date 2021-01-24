module Geometry.CardStacks (cardAtStack, freeCells, goalCells, cardPresencePosition, cardNumInfo, cardNumberSize) where

import Geometry.BoardPositions (Position(..))
import Geometry.BoardRegions (Region(..), offset, bottomCenter)

-- There are 8 card stacks side by side
-- Any card stack is 0 to 13 cards deep
cardAtStack :: Int -> Int -> Region 
cardAtStack stackNum depth = (cardStacks !! stackNum) `offset` (Pos 0 depth * cardStackOffset)

cardStacks :: [Region]
cardStacks = map (\n -> offset cardStack0 (Pos n 0 * adjacentCardOffset)) $ take 8 [0..]

cardStack0 :: Region
cardStack0 = offset cardSize (Pos 407 392)


-- There are 3 free cells that can hold one of any card each
-- One complete set of 4 matching Dragons can be permanently be moved into a single free cell
freeCells :: [Region]
freeCells = map (\n -> offset freeCell0 (Pos n 0 * adjacentCardOffset)) $ take 3 [0..]

freeCell0 :: Region
freeCell0 = offset cardSize (Pos 407 128)


-- There are 3 goal stacks that start empty, and require cards from 1 to 9 to be added for each distinct suit
-- The cards are moved to these goals automatically when it is trivial/safe to do so, but they may also be moved manually
goalCells :: [Region]
goalCells = map (\n -> offset goalCell0 (Pos n 0 * adjacentCardOffset)) $ take 3 [0..]

goalCell0 :: Region 
goalCell0 = offset cardSize (Pos 1163 125)


cardSize :: Region
cardSize = Reg { pos1 = Pos 0 0, pos2 = Pos 115 225 }

cardNumInfo :: Region 
cardNumInfo = offset cardNumberSize cardInfoOffset


cardStackOffset :: Position
cardStackOffset = Pos 0 31

adjacentCardOffset :: Position
adjacentCardOffset = Pos 152 0

cardInfoOffset :: Position
cardInfoOffset = Pos 4 4 

cardPresencePosition :: Position 
cardPresencePosition = bottomCenter cardSize

cardNumberSize :: Region 
cardNumberSize = Reg { pos1 = Pos 0 0, pos2 = Pos 17 22 }
cardSuitSize :: Region 
cardSuitSize = Reg { pos1 = Pos 0 0, pos2 = Pos 15 15 }
cardSpecialSuitSize :: Region 
cardSpecialSuitSize = Reg { pos1 = Pos 0 0, pos2 = Pos 20 24 }