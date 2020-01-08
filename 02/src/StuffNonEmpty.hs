module StuffNonEmpty
  ( NonEmpty(..)
  , mapNonEmpty
  , groupNonEmpty
  , groupByNonEmpty
  , groupOnNonEmpty
  , classifyOnNonEmpty
  ) where

import Stuff (sortOn, sortBy, on, (&&&))

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty = groupByNonEmpty (==)

data NonEmpty a = a :| [a]
  deriving (Show, Eq, Ord)
infixr 4 :|

mapNonEmpty :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNonEmpty f (x :| xs) = f x :| map f xs

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty _ [] = []
groupByNonEmpty f (x:xs) = (x :| h) : groupByNonEmpty f t
  where (h, t) = span (f x) xs

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f =
  map (mapNonEmpty snd)
  . groupByNonEmpty ((==) `on` fst) 
  . map (f &&& id)

classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f =
  map (mapNonEmpty snd)
  . groupByNonEmpty ((==) `on` fst)
  . sortBy (compare `on` fst)
  . map (f &&& id)