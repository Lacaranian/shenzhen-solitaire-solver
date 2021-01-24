{-# LANGUAGE TupleSections #-}
module Game.State where

import Data.Bifunctor (first)
import Data.Maybe (catMaybes, mapMaybe, listToMaybe, maybeToList)

import Geometry.BoardRegions
import Geometry.CardStacks
import Util ((!!?), lastOption, flipMaybe, windows, takeWhilst, count)


data Game = Game { freeCellCards :: [Cell], goalCellCards :: [Cell], cardStackCards :: [[Card]] } deriving (Eq, Show)

data Cell = Empty 
          | CellCard Card 
          | Complete 
          deriving (Eq, Show)

data Card = Card Int CardSuit 
          | Dragon DragonSuit 
          | Rose 
          deriving (Eq, Show)

data CardSuit = Stones     -- Red
              | Sticks     -- Green
              | Characters -- Black
              deriving (Eq, Show)

data DragonSuit = Red   -- Arrow
                | Green -- Character
                | White -- Square
                deriving (Eq, Show)


data GamePosition = FreeCellSlot Int 
                  | GoalCellSlot Int
                  | CardStackSlot Int Int -- 0 indexed from an empty stack
                  deriving (Eq, Show)

data CardSlotContents = OneCard Card | NoCard

cardAtPosition :: Game -> GamePosition -> Maybe Card
cardAtPosition game (FreeCellSlot idx)          = maybeCardInCell =<< freeCellCards game !!? idx
cardAtPosition game (GoalCellSlot idx)          = maybeCardInCell =<< goalCellCards game !!? idx
cardAtPosition game (CardStackSlot stackID idx) = do 
    stack <- cardStackCards game !!? stackID
    stack !!? idx

maybeCardInCell :: Cell -> Maybe Card
maybeCardInCell (CellCard card) = Just card
maybeCardInCell _               = Nothing

cellFromMaybeCard :: Maybe Card -> Cell
cellFromMaybeCard (Just card) = CellCard card
cellFromMaybeCard Nothing     = Empty

regionForGamePosition :: GamePosition -> Region
regionForGamePosition (FreeCellSlot idx) = freeCells !! idx
regionForGamePosition (GoalCellSlot idx) = goalCells !! idx
regionForGamePosition (CardStackSlot stackID idx) = cardAtStack stackID idx 

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

matchesNumAndSuit :: Foldable t => Int -> t CardSuit -> Card -> Bool
matchesNumAndSuit reqNum maybeSuit (Card num suit) = num == reqNum && all (== suit) maybeSuit
matchesNumAndSuit reqNum maybeSuit _               = False

windowIsInSubstack :: [Maybe (a, Card)] -> Bool
windowIsInSubstack (Nothing : Just (_, upper) : _)          = True
windowIsInSubstack (Just (_, lower) : Just (_, upper) : _)  = compatibleInSubstack lower upper
windowIsInSubstack _                                        = False

-- Can the first card stack on top of the second card?
compatibleInSubstack :: Card -> Card -> Bool
compatibleInSubstack (Card num1 suit1) (Card num2 suit2) = num1 + 1 == num2 && suit1 /= suit2
compatibleInSubstack _ _ = False

exposedFreeCellCards :: Game -> [(GamePosition, Card)]
exposedFreeCellCards game = mapMaybe (fmap (first FreeCellSlot) . cardWithPosInCell) $ zip [0..] $ freeCellCards game
    where 
        cardWithPosInCell (idx, CellCard card) = Just (idx, card)
        cardWithPosInCell _                    = Nothing

openFreeCellSlots :: Game -> [GamePosition]
openFreeCellSlots = mapMaybe (fmap FreeCellSlot . asFreeSlot) . zip [0..] . freeCellCards
    where 
        asFreeSlot (idx, Empty) = Just idx
        asFreeSlot _            = Nothing

isDragon :: Card -> Bool 
isDragon (Dragon _) = True 
isDragon _          = False

isFreeCellPosition :: GamePosition -> Bool 
isFreeCellPosition (FreeCellSlot _) = True 
isFreeCellPosition _                = False

stacksOn :: Card -> Card -> Bool
stacksOn (Card num suit) (Card num2 suit2) = num + 1 == num2 && suit /= suit2  
stacksOn _ _ = False

numEmptySlots :: Game -> Int
numEmptySlots game = length (openFreeCellSlots game) + count (==[]) (cardStackCards game)

exposedSuitDragons :: Game -> DragonSuit -> [(GamePosition, Card)]
exposedSuitDragons game dragonSuit = do
    let exposedCards = exposedFreeCellCards game ++ exposedStackCards game
    filter (isDragonOfSuit dragonSuit . snd) exposedCards
    where 
        isDragonOfSuit reqSuit (Dragon suit) = suit == reqSuit
        isDragonOfSuit _ _ = False

didWin :: Game -> Bool
didWin game = all (==[]) $ cardStackCards game