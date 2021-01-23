{-# LANGUAGE TupleSections #-}
module Game.AI where

import Game.Actions
import Game.State
import Util ( maxOn, headOption )

-- Any AI chooses one of the possible actions, or chooses to give up
-- Game -> [Action] -> Maybe Action

-- A trivial AI, just picks the first valid action it can take, always
firstMoveAI :: Game -> [Action] -> Maybe Action 
firstMoveAI _ = headOption

-- A somewhat dumb AI, stops as soon as it would incur costs by reducing its total moves
-- Usually does nothing
maximizeMovesAI :: Game -> [Action] -> Maybe Action
maximizeMovesAI game [] = Nothing
maximizeMovesAI game curActs =  if currentCount < fst bestNextCountAct then Just $ snd bestNextCountAct else Nothing
    where 
        currentCount = length curActs
        bestNextCountAct = maxOn fst actsWithNextCount
        actsWithNextCount = map (\act -> (,act) . length . availableActions $ updateGame act game) curActs
