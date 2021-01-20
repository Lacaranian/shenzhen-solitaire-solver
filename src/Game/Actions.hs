{-# LANGUAGE TupleSections #-}
module Game.Actions where

import Data.Bifunctor (bimap, first, second)
import Data.List (find)
import Data.Maybe (maybe, catMaybes, mapMaybe, listToMaybe, maybeToList, fromMaybe)

import Game.State
import Geometry.BoardRegions
import Geometry.Buttons
import Geometry.CardStacks
import Util (lastOption, windows, takeWhilst, insertedAt, removedAt, replacedAt)
import XDoTool

-- TODO - interleave a score with every Action
data Action = MoveAct Move 
            | CompleteDragon DragonCompletion
            | MoveRose GamePosition
            | NewGame
            deriving (Eq, Show)

data Move = Move { sourcePosition :: GamePosition, destinationPosition :: GamePosition } deriving (Eq, Show)
data DragonCompletion = DragonCompletion { forSuit :: DragonSuit, dragons :: [GamePosition], targetFreeCell :: GamePosition } deriving (Eq, Show)

availableActions :: Game -> [Action]
availableActions game = map MoveAct (availableMoves game) ++ map CompleteDragon (availableDragonCompletions game) -- TODO add new game as a last resort?

availableMoves :: Game -> [Move]
availableMoves game = movesToGoalCells game ++ movesToStacks game ++ movesToFreeCells game

-- If there is an open free cell, or there is at least one dragon
availableDragonCompletions :: Game -> [DragonCompletion]
availableDragonCompletions game = do
    let exposedCards = exposedFreeCellCards game ++ exposedStackCards game
    dragonSuit <- [Red, Green, White]
    let exposedDragonsOfSuit = filter (isDragonOfSuit dragonSuit . snd) exposedCards
    targetFreeCell <- maybeToList (find isFreeCellPosition (map fst exposedDragonsOfSuit)) ++ openFreeCellSlots game
    [DragonCompletion dragonSuit (map fst exposedDragonsOfSuit) targetFreeCell | length exposedDragonsOfSuit == 4]
    where 
        isDragonOfSuit reqSuit (Dragon suit) = suit == reqSuit
        isDragonOfSuit _ _ = False

-- Move any exposed stack card to an empty free cell
movesToFreeCells :: Game -> [Move]
movesToFreeCells game = do
    destination <- openFreeCellSlots game
    source <- exposedStackCardPositions game
    return $ Move source destination 

-- Move any exposed single card to a goal cell with a card of matching suit and 1 less value (or any 1 to any empty slot)
movesToGoalCells :: Game -> [Move]
movesToGoalCells game = do 
    (goalSlot, maybeCard) <- zipWith (\idx slot -> (GoalCellSlot idx, slot)) [0..] $ goalCellCards game
    let (requiredNum, requiredSuit) = maybe (1, Nothing) (\(Card num suit) -> (num + 1, Just suit)) maybeCard
    (availablePosition, _) <- filter (matchesNumAndSuit requiredNum requiredSuit . snd) $ exposedStackCards game ++ exposedFreeCellCards game
    return $ Move availablePosition goalSlot 

-- Moving any free cell card or stack to either
--   - an empty card stack
--   - a target stack where the target card is 1 greater value, and a different suit, than the base of the stack 
movesToStacks :: Game -> [Move]
movesToStacks game = do
    (destStackPosition, maybeDestCard) <- map (second Just) (exposedStackCards game) ++ map (, Nothing) (emptyStacks game) 
    (sourceCardPosition, sourceCard) <- exposedStackSubstacks game ++ exposedFreeCellCards game
    [Move sourceCardPosition destStackPosition | all (sourceCard `stacksOn`) maybeDestCard]

execWithUpdate :: Game -> Action -> IO Game
execWithUpdate game act = do
    exec act
    return $ updateGame act game

exec :: Action -> IO ()
exec (MoveAct (Move sourcePos destPos))            = drag (topCenter $ regionForGamePosition sourcePos) (topCenter $ regionForGamePosition destPos)
exec (CompleteDragon (DragonCompletion suit _ _ )) = clickAt $ center $ dragonButtonForSuit suit
exec NewGame                                       = clickAt $ center newGame

-- The expected new game state after an action is run
-- Notably, after many actions, uncovered cards will automatically move to goal stacks on their own, so this must account for those
updateGame :: Action -> Game -> Game
updateGame act game = foldl (flip updateGame) steppedGame $ automaticActions steppedGame
    where steppedGame = updateGameOnce act game

updateGameOnce :: Action -> Game -> Game
-- TODO: Moving stacks needs to move multiple cards at once
updateGameOnce (MoveAct (Move src dest)) game = addedAtPosition dest card . removedAtPosition src $ game
    where card = fromMaybe Rose $ cardAtPosition game src
updateGameOnce (MoveRose pos) game = removedAtPosition pos game
updateGameOnce (CompleteDragon (DragonCompletion _ drags dest)) game = removePosition dest . foldl (flip removedAtPosition) game $ drags
updateGameOnce NewGame game = undefined -- Randomized new game state that has to be screenshotted

addedAtPosition :: GamePosition -> Card -> Game -> Game
addedAtPosition (FreeCellSlot idx)          card game = game { freeCellCards = replacedAt idx (Just card) $ freeCellCards game} 
addedAtPosition (CardStackSlot stackID idx) card game = game { cardStackCards = replacedAt stackID (insertedAt idx card $ cscs !! stackID) cscs }
    where cscs = cardStackCards game
addedAtPosition (GoalCellSlot idx)          card game = game { goalCellCards = replacedAt idx (Just card) $ goalCellCards game} 

removedAtPosition :: GamePosition -> Game -> Game
removedAtPosition (FreeCellSlot idx)          game = game { freeCellCards = replacedAt idx Nothing $ freeCellCards game} 
removedAtPosition (CardStackSlot stackID idx) game = game { cardStackCards = replacedAt stackID (removedAt idx $ cscs !! stackID) cscs }
    where cscs = cardStackCards game
-- Impossible to remove goal cell cards once moved, though it would be a card of matching suit and value 1 less, or else an empty cell
removedAtPosition (GoalCellSlot idx)          game = undefined

removePosition :: GamePosition -> Game -> Game
removePosition (FreeCellSlot idx) game = game { freeCellCards = removedAt idx $ freeCellCards game }
removePosition _                  _    = undefined

-- The Rose always moves to its special cell once uncovered
--
-- When are moves to goal cells automatic? 
-- Appears to be when 
-- - there exists a free cell with a possible move
-- - all cards of number 1 less than a candidate to automove could themselves automove, once uncovered
-- Aka, the lowest numbered card in the goal cells (0 for empties)
automaticActions :: Game -> [Action]
automaticActions game = maybeToList moveRose ++ autoMoves
    where 
        moveRose  = fmap (MoveRose . fst) . find ((== Rose) . snd) $ exposedStackCards game
        autoMoves = map MoveAct . filter (isAutoMove . cardAtPosition game . sourcePosition) $ movesToGoalCells game
        autoMoveNum = maximum $ map (maybe 2 (\(Card num _) -> num + 2)) $ goalCellCards game
        isAutoMove (Just (Card num _)) = num <= autoMoveNum
        isAutoMove _ = False