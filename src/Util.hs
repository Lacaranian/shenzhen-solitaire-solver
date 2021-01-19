module Util where

import Data.List ( transpose, tails)

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