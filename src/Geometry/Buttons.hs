module Geometry.Buttons where

import Geometry.BoardPositions (Position(..))
import Geometry.BoardRegions (Region(..), offset)


redDragon :: Region 
redDragon = Reg { pos1 = Pos 870 143, pos2 = Pos 906 173 }

greenDragon :: Region 
greenDragon = offset redDragon dragonButtonOffset

whiteDragon :: Region 
whiteDragon = offset redDragon (Pos 0 2 * dragonButtonOffset)

dragonButtonOffset :: Position
dragonButtonOffset = Pos 0 80


newGame :: Region 
newGame = Reg { pos1 = Pos 1397 915, pos2 = Pos 1631 952 }

