module Util where

import Data.List (transpose, tails)
import Data.Maybe ( isNothing ) 
import Data.Foldable (maximumBy)
import Data.Ord      (comparing)

-- safer O(n) version of (!!)
(!!?) :: [a] -> Int -> Maybe a
(!!?) []       _ = Nothing
(!!?) (x : _)  0 = Just x
(!!?) (x : xs) n = xs !!? (n - 1)

-- safer versions of head/last
headOption :: [a] -> Maybe a
headOption [] = Nothing 
headOption (x : xs) = Just x

lastOption :: [a] -> Maybe a
lastOption = headOption . reverse

takeWhilst :: Foldable t => (a -> Bool) -> t a -> [a]
takeWhilst f = foldr (\x acc -> if f x then x : acc else []) []

windows :: Int -> [a] -> [[a]]
windows n = transpose . take n . tails

insertedAt :: Int -> a -> [a] -> [a]
insertedAt n x xs = let (ys, zs) = splitAt n xs in ys ++ [x] ++ zs

removedAt :: Int -> [a] -> [a]
removedAt 0   (x : xs) = xs
removedAt idx []       = undefined -- Can't access indices out of bounds
removedAt idx (x : xs) = x : removedAt (idx - 1) xs

replacedAt :: Int -> a -> [a] -> [a]
replacedAt 0   newValue (x : xs) = newValue : xs
replacedAt idx newValue []       = undefined -- Can't access indices out of bounds
replacedAt idx newValue (x : xs) = x : replacedAt (idx - 1) newValue xs

maxOnOption :: (Foldable t, Ord a) => (b -> a) -> t b -> Maybe b
maxOnOption func foldable  = if null foldable then Nothing else Just $ (maximumBy . comparing) func foldable

firstJust :: [a -> Maybe b] -> a -> Maybe b
firstJust maybeGens seed = foldr (\gen value -> if isNothing value then gen seed else value) Nothing maybeGens

flipMaybe :: b -> Maybe a -> Maybe b
flipMaybe _  (Just _) = Nothing
flipMaybe def Nothing = Just def

count :: (a -> Bool) -> [a] -> Int 
count pred xs = length $ filter pred xs