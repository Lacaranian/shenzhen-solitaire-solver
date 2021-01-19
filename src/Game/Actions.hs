{-# LANGUAGE TupleSections #-}
module Game.Actions where

import Data.Bifunctor (bimap, first, second)
import Data.Maybe (catMaybes, mapMaybe, listToMaybe, maybeToList)

import Game.State (Game(..), Card(..), CardSuit(..), DragonSuit(..), GamePosition(..))
import Util (lastOption, windows, takeWhilst)

data Action = MoveAct Move | CompleteDragon DragonSuit | NewGame

data Move = Move { sourcePosition :: GamePosition, destinationPosition :: GamePosition }

availableMoves :: Game -> [Move]
availableMoves game = movesToFreeCells game ++ movesToGoalCells game ++ movesToStacks game

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

stacksOn :: Card -> Card -> Bool
stacksOn (Card num suit) (Card num2 suit2) = num + 1 == num2 && suit /= suit2  
stacksOn _ _ = False

matchesNumAndSuit :: Foldable t => Int -> t CardSuit -> Card -> Bool
matchesNumAndSuit reqNum maybeSuit (Card num suit) = num == reqNum && all (== suit) maybeSuit
matchesNumAndSuit reqNum maybeSuit _               = False

-- Cards that are always movable, by virtue of being the lowest card in any given stack
exposedStackCardPositions :: Game -> [GamePosition]
exposedStackCardPositions = map fst . exposedStackCards

exposedStackCards :: Game -> [(GamePosition, Card)]
exposedStackCards game = do
    (stackID, stack) <- zip [0..] $ cardStackCards game
    maybeToList . lastOption $ zipWith (\idx card -> (CardStackSlot stackID idx, card)) [0..] stack

-- All movable parts of a stack, from bottom up to the last card that counts as a substack
exposedStackSubstacks :: Game -> [(GamePosition, Card)]
exposedStackSubstacks game = do
    (stackID, stack) <- zip [0..] $ cardStackCards game
    let indexedRevStack = reverse . zip [0..] $ stack
    let positionedRevStack = map (Just . first  (CardStackSlot stackID)) indexedRevStack
    let allWindows = windows 2 (Nothing : positionedRevStack)
    let substackWindows = takeWhilst windowIsInSubstack allWindows 
    catMaybes . mapMaybe (listToMaybe . drop 1) $ substackWindows

emptyStacks :: Game -> [GamePosition]
emptyStacks game = do
    (stackID, stack) <- zip [0..] $ cardStackCards game
    [CardStackSlot stackID 0 | null stack]

windowIsInSubstack :: [Maybe (a, Card)] -> Bool
windowIsInSubstack (Nothing : Just (_, upper) : _)          = True
windowIsInSubstack (Just (_, lower) : Just (_, upper) : _)  = compatibleInSubstack lower upper
windowIsInSubstack _                                        = False


-- Can the first card stack on top of the second card?
compatibleInSubstack :: Card -> Card -> Bool
compatibleInSubstack (Card num1 suit1) (Card num2 suit2) = num1 + 1 == num2 && suit1 /= suit2
compatibleInSubstack _ _ = False

exposedFreeCellCards :: Game -> [(GamePosition, Card)]
exposedFreeCellCards game = mapMaybe (asFilled FreeCellSlot) . zip [0..] $ freeCellCards game
        
openFreeCellSlots :: Game -> [GamePosition]
openFreeCellSlots = mapMaybe (asOpen FreeCellSlot) . zip [0..] . freeCellCards

asFilled :: (Int -> GamePosition) -> (Int, Maybe a) -> Maybe (GamePosition, a)
asFilled posGen (idx, Just card) = Just (posGen idx, card)
asFilled posGen (idx, Nothing)   = Nothing

asOpen :: (Int -> GamePosition) -> (Int, Maybe a) -> Maybe GamePosition
asOpen posGen (idx, Just _)  = Nothing
asOpen posGen (idx, Nothing) = Just (posGen idx)
