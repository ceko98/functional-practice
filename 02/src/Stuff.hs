module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where

takeBy :: (a -> a -> Bool) -> [a] -> [a]
takeBy _ [] = []
takeBy _ [x] = [x]
takeBy f (x:y:xs) = if x `f` y then x : (takeBy f (y:xs)) else [x]

dropBy :: (a -> a -> Bool) -> [a] -> [a]
dropBy _ [] = []
dropBy _ [_] = []
dropBy f (x:y:xs) = if x `f` y then dropBy f (y:xs) else (y:xs)

group :: Eq a => [a] -> [[a]]
group [] = []
group xs = takeBy (==) xs : (group (dropBy (==) xs))

-- Not mandatory, delete if you don't want this.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ a [] = [a]
insertBy f a (x:xs) = if f a x == GT then x : (insertBy f a xs) else a:x:xs

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:xs) = insertBy f x (sortBy f xs)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f xs = takeBy f xs : (groupBy f (dropBy f xs))

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f 
  = map snd
  . sortBy (compare `on` fst) 
  . map (f &&& id)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f
  = map (map snd)
    . groupBy ((==) `on` fst) 
    . map (f &&& id)

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f = groupOn f . sortOn f
