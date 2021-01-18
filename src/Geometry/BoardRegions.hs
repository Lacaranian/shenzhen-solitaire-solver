module Geometry.BoardRegions where

import Geometry.BoardPositions (Position(..))

-- Areas where certain game objects are expected to be found
-- Assumes 1920x1080 screenshot/pixel buffer size (can make percentages if this changes? depending on how different resolutions change layout)

-- (x1, y1, x2, y2) for a rectangular region 
-- replace when a good datatype from a dependency is found?
data Region = Reg { pos1 :: Position , pos2 :: Position }

center :: Region -> Position
center (Reg pos1 pos2) = Pos ((x pos1 + x pos2) `div` 2) ((y pos1 + y pos2) `div` 2)

bottomCenter :: Region -> Position
bottomCenter (Reg pos1 pos2) = Pos ((x pos1 + x pos2) `div` 2) ((y pos1 + (9 * y pos2)) `div` 10)

offset :: Region -> Position -> Region
offset (Reg pos1 pos2) pos =  Reg (pos1 + pos) (pos2 + pos)
