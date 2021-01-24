{-# LANGUAGE TupleSections #-}
module Game.Actions where

import Control.Monad (guard)
import Data.Bifunctor (bimap, first, second)
import Data.List (find)
import Data.Maybe (maybe, catMaybes, mapMaybe, listToMaybe, maybeToList, fromMaybe, isJust)

import Game.State
import Geometry.BoardRegions
import Geometry.Buttons
import Geometry.CardStacks
import Util (headOption, lastOption, windows, takeWhilst, insertedAt, removedAt, replacedAt)
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
    dragonSuit <- [Red, Green, White]
    let exposedCards = exposedFreeCellCards game ++ exposedStackCards game
    let exposedDragonsOfSuit = filter (isDragonOfSuit dragonSuit . snd) exposedCards
    targetFreeCell <- maybeToList (find isFreeCellPosition (map fst exposedDragonsOfSuit)) ++ openFreeCellSlots game
    guard . (== 4) . length $ exposedDragonsOfSuit 
    return $ DragonCompletion dragonSuit (map fst exposedDragonsOfSuit) targetFreeCell
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
    (dest@(CardStackSlot destStackID destCardIdx), maybeDestCard) <- map (second Just) (exposedStackCards game) ++ map (, Nothing) (emptyStacks game)
    let actualDestPosition = if isJust maybeDestCard then CardStackSlot destStackID (destCardIdx + 1) else dest
    (sourceCardPosition, sourceCard) <- exposedStackSubstacks game ++ exposedFreeCellCards game
    [Move sourceCardPosition actualDestPosition | all (sourceCard `stacksOn`) maybeDestCard]

execWithUpdate :: Game -> Action -> IO Game
execWithUpdate game act = do
    let newGame = updateGame act game
    exec act
    return newGame

exec :: Action -> IO ()
exec (MoveAct (Move sourcePos destPos))            = drag (topCenter $ regionForGamePosition sourcePos) (topCenter $ regionForGamePosition destPos)
exec (CompleteDragon (DragonCompletion suit _ _ )) = clickAt $ center $ dragonButtonForSuit suit
exec NewGame                                       = clickAt $ center newGame

-- The expected new game state after an action is run
-- Notably, after many actions, uncovered cards will automatically move to goal stacks on their own, so this must account for those
updateGame :: Action -> Game -> Game
updateGame act game = autoUpdated steppedGame
    where steppedGame = updateGameOnce game act

updateGameOnce :: Game -> Action -> Game
-- TODO: Moving stacks needs to move multiple cards at once
updateGameOnce game (MoveAct (Move src@(CardStackSlot stackID idx) dest))
    | idx + 1 < length (cardStackCards game !! stackID) = moveSubstack src dest game
updateGameOnce game (MoveAct (Move src dest)) = addedAtPosition dest card . removedAtPosition src $ game
    where card = fromMaybe Rose $ cardAtPosition game src
updateGameOnce game (MoveRose pos) = removedAtPosition pos game
updateGameOnce game (CompleteDragon (DragonCompletion _ drags dest)) = removePosition dest . foldl (flip removedAtPosition) game $ drags
updateGameOnce game NewGame = undefined -- Randomized new game state that has to be screenshotted

addedAtPosition :: GamePosition -> Card -> Game -> Game
addedAtPosition (FreeCellSlot idx)          card game = game { freeCellCards = replacedAt idx (Just card) $ freeCellCards game} 
addedAtPosition (CardStackSlot stackID idx) card game = game { cardStackCards = replacedAt stackID (insertedAt idx card $ cscs !! stackID) cscs } -- TODO +1 to destIdx here, or in use sites?
    where cscs = cardStackCards game
addedAtPosition (GoalCellSlot idx)          card game = game { goalCellCards = replacedAt idx (Just card) $ goalCellCards game} 

removedAtPosition :: GamePosition -> Game -> Game
removedAtPosition (FreeCellSlot idx)          game = game { freeCellCards = replacedAt idx Nothing $ freeCellCards game} 
removedAtPosition (CardStackSlot stackID idx) game = game { cardStackCards = replacedAt stackID (removedAt idx $ cscs !! stackID) cscs } -- HERE, removed in stack 0 idx 2, after there's only 2 there (idx 0 and 1)!
    where cscs = cardStackCards game
-- Impossible to remove goal cell cards once moved, though it would be a card of matching suit and value 1 less, or else an empty cell
removedAtPosition (GoalCellSlot idx)          game = undefined

removePosition :: GamePosition -> Game -> Game
removePosition (FreeCellSlot idx) game = game { freeCellCards = removedAt idx $ freeCellCards game }
removePosition _                  _    = undefined

moveSubstack :: GamePosition -> GamePosition -> Game -> Game
moveSubstack src@(CardStackSlot stackID idx) (CardStackSlot destStackID destIdx) game = foldl (\newGame move -> move newGame) game moves
    where 
        moves = map moveSingle [destIdx .. destIdx + substackLength - 1]
        moveSingle newDestIdx = uncurry (addedAtPosition (CardStackSlot destStackID newDestIdx)) . second (removedAtPosition src) . withSrcCard
        withSrcCard  newGame = (fromMaybe Rose $ cardAtPosition newGame src, newGame)
        substackLength = sourceStacklength - idx
        sourceStacklength = length (cardStackCards game !! stackID)
moveSubstack _ _ _ = undefined -- Substacks only move around on the main board

autoUpdated :: Game -> Game
autoUpdated = fst . autoUpdates

autoUpdates :: Game -> (Game, [Action])
autoUpdates game = autoUpdatesFromNext $ nextAutoAction game
    where 
        autoUpdatesFromNext (Just act) = 
            let nextGame = updateGameOnce game act in 
            let (finalGame, nextUpdates) = autoUpdates nextGame in 
                (finalGame, act : nextUpdates)
        autoUpdatesFromNext Nothing    = (game, [])
        nextAutoAction = headOption . automaticActions


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

isInverseAction :: Action -> Action -> Bool
isInverseAction (MoveAct (Move src dest)) (MoveAct (Move otherSrc otherDest)) = src == otherDest && dest == otherSrc
isInverseAction act otherAct = False