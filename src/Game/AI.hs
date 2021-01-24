{-# LANGUAGE TupleSections #-}
module Game.AI where

import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes, fromJust, isJust)

import Game.Actions
import Game.State
import Util ( maxOn, maxOnOption, headOption, count )

-- Any AI chooses one of the possible actions, or chooses to give up
-- Game -> [Action] -> Maybe Action
type AI = Game -> [Action] -> Maybe Action
type AIFilter = Game -> [Action] -> [Action]



-- A trivial AI, just picks the first valid action it can take, always
-- Often ends up in loops, where one move undoes the move directly preceding it 
firstMoveAI :: AI
firstMoveAI _ = headOption

-- A somewhat dumb AI, stops as soon as it would incur costs by reducing its total moves
-- Usually does nothing
maximizeMovesAI :: AI
maximizeMovesAI game [] = Nothing
maximizeMovesAI game curActs =  if currentCount < fst bestNextCountAct then Just $ snd bestNextCountAct else Nothing
    where 
        currentCount = length curActs
        bestNextCountAct = maxOn fst actsWithNextCount
        actsWithNextCount = map (\act -> (,act) . length . availableActions $ updateGame act game) curActs

-- Some AI do better if some options are out-of-bounds for them
withAIFilter :: AIFilter -> AI -> AI
withAIFilter filt ai game acts = ai game (filt game acts)

-- If the preferred move would lead immediately to a move that reverses what it just did, it should not be allowed
-- Does not work well or at all in practice, since it examines only one layer deep
-- To have the nextMoves also include antiDirectLooping requires arbitrary (infinite) lookahead
antiDirectLoopingFilter :: AI -> AI
antiDirectLoopingFilter ai game availActs = ai game remainingAvilableActions
    where
        remainingAvilableActions = catMaybes $ zipWith withoutInverseAct availActs nextMoves
        withoutInverseAct now (Just next) = if isInverseAction now next then Nothing else Just now
        withoutInverseAct now _ = Just now
        nextMoves = map ((\newGame -> ai newGame $ availableActions newGame) . (`updateGame` game)) availActs

scoredAI :: AI
scoredAI game [] = Nothing
scoredAI game curActs = fmap fst . maxOnOption snd $ goodActionsWithScores
    where goodActionsWithScores = [(act, score) | act <- curActs, 
                                                  let score = actionScore game act, 
                                                  score > 0 ]
    

-- Includes this action plus any automatic actions to follow
actionScore :: Game -> Action -> Int
actionScore game action = steppedScore + autoScore
    where 
        autoScore = fst . foldl (\(curScore, curGame) act -> (curScore + actionScoreOnce curGame act, updateGameOnce curGame act)) (0, steppedGame) $ autoActs
        (steppedGame, steppedScore) = bimap (\f -> f game action) (\f -> f game action) (updateGameOnce, actionScoreOnce)
        (finalGame , autoActs) = autoUpdates steppedGame

actionScoreOnce :: Game -> Action -> Int
actionScoreOnce game (MoveAct (Move src dest)) = moveScore game src dest
actionScoreOnce game (CompleteDragon (DragonCompletion suit dragons dest)) = removePositionScore game dest + (sum . map (uncoverScore game) $ dragons)
actionScoreOnce game (MoveRose src) = 10
actionScoreOnce game NewGame = -1

moveScore :: Game -> GamePosition -> GamePosition -> Int
moveScore game src dest = uncoverScore game src + coverScore game srcCard (coveredPos dest)
    where 
        srcCard = fromJust $ cardAtPosition game src
        coveredPos (CardStackSlot stackID idx) = CardStackSlot stackID $ max 0 (idx - 1)
        coveredPos destCell = destCell

-- For cells, the position is the uncovered one (and leaves the cell empty)
-- For stacks, the position is one more than the uncovered one (unless at 0)
-- !!! TODO !!! Add valuation for uncovering the fourth dragon for a dragon completion
uncoverScore :: Game -> GamePosition -> Int
uncoverScore game (FreeCellSlot idx) = scoreForChangeInEmptySlots 1 $ numEmptySlots game
uncoverScore game src@(CardStackSlot stackID idx) = valueOfSubstacking . not $ positionIsInSubstack game src
uncoverScore game _ = undefined -- goal slots are never uncovered

coverScore :: Game -> Card -> GamePosition -> Int
coverScore game card (GoalCellSlot slot) = 10
coverScore game card (FreeCellSlot idx) = scoreForChangeInEmptySlots (-1) $ numEmptySlots game
coverScore game card dest@(CardStackSlot stackID 0) | isJust . cardAtPosition game $ dest = scoreForChangeInEmptySlots (-1) $ numEmptySlots game
coverScore game card dest@(CardStackSlot _ _) = valueOfSubstacking $ positionIsInSubstack game dest

-- Requires a card be at the specified position
positionIsInSubstack :: Game -> GamePosition -> Bool
positionIsInSubstack game src@(CardStackSlot stackID idx) = any (compatibleInSubstack . fromJust $ cardAtPosition game src) uncoveredCard
    where uncoveredCard = cardAtPosition game (CardStackSlot stackID $ max 0 (idx - 1))
positionIsInSubstack _ _ = False

-- For removing free cells from play, when completing dragons
removePositionScore :: Game -> GamePosition -> Int
removePositionScore game pos = betterIfAlreadyFull $ cardAtPosition game pos 
    where 
        betterIfAlreadyFull (Just _) = 0
        betterIfAlreadyFull Nothing  = scoreForChangeInEmptySlots (-1) . numEmptySlots $ game

scoreForChangeInEmptySlots :: (Num p, Eq p) => p -> p -> p
scoreForChangeInEmptySlots diff curSlots = valueOfEmptySlots (curSlots + diff) - valueOfEmptySlots curSlots


valueOfSubstacking :: Bool -> Int 
valueOfSubstacking True = 3
valueOfSubstacking False = 0

valueOfEmptySlots :: (Eq p, Num p) => p -> p
valueOfEmptySlots 0 = 0
valueOfEmptySlots 1 = 5
valueOfEmptySlots 2 = 8
valueOfEmptySlots 3 = 10
valueOfEmptySlots 4 = 11
valueOfEmptySlots x = x + 7


-- TODO
-- Dont move a substack already on top of a substack compatibe number, onto another substack compatible number, 
-- UNLESS there's another matching top substack number that is not already on a substack number, and that one can't move
-- eg. a 4 on a 5 should not move to another 5, unless it'd make room for another 4

-- Can we guarantee a game terminates? 
-- Not easily, though probably...
-- Simply having no moves left is not a good metric 
--
-- Due to the non-undoable nature of many actions in the game, a game can enter a set of "valley" states,  where it can make 
-- moves that keep it in the valley, but not leave the valley. If the game is seen as graph, with the goal node not reachable
-- from every node that itself is reachable from an initial node, those disconnected branches can be seen as these valleys.
