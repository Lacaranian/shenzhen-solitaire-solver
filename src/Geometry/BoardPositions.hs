module Geometry.BoardPositions where

data Position = Pos { x :: Int, y :: Int }

instance Num Position where
   Pos a b + Pos c d = Pos (a+c) (b+d)
   Pos a b * Pos c d = Pos (a*c) (b*d)
   Pos a b - Pos c d = Pos (a-c) (b-d)
   abs    (Pos a b) = Pos (abs a) (abs b) 
   signum (Pos a b) = Pos (signum a) (signum b) 
   fromInteger i = Pos (fromInteger i) (fromInteger i)

freeCell1 :: Position
freeCell1 = Pos { x = 450, y = 250 }