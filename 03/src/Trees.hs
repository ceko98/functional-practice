{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  Node x ltr1 rtr1 == Node y ltr2 rtr2 = x == y && ltr1 == ltr2 && rtr1 == rtr2
  _ == _ = False


insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered x Empty = Node x Empty Empty
insertOrdered x (Node a ltr rtr) = if x <= a
  then Node a (insertOrdered x ltr) rtr
  else Node a ltr (insertOrdered x rtr)

listToBST :: Ord a => [a] -> Tree a
listToBST = foldr insertOrdered Empty

isBST :: Ord a => Tree a -> Bool
isBST = between Bot Top

-- idea for implementing isBST - delete if you don't want it
data BotTop a = Bot | Val a | Top
  deriving (Show, Eq, Ord)

between :: Ord a => BotTop a -> BotTop a -> Tree a -> Bool
between a b (Node x ltr rtr) = a <= v && v <= b && between a v ltr && between v b rtr
  where v = Val x
between _ _ Empty = True

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST x (Node a ltr rtr) = x == a || findBST x ltr || findBST x rtr

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node a ltr rtr) = Node (f a) (mapTree f ltr) (mapTree f rtr)

foldTree :: Monoid a => Tree a -> a
foldTree Empty = mempty
foldTree (Node a ltr rtr) = foldTree ltr <> a <> foldTree rtr

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree f = foldTree . mapTree f

sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMapTree Sum

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f = getAll . foldMapTree (All . f)

treeToList :: Tree a -> [a]
treeToList = foldMapTree (:[])

elemTree :: Eq a => a -> Tree a -> Bool
elemTree x = getAny . foldMapTree (Any . (==) x)

onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred f = getFirst . foldMapTree (First . onMaybe f)

findAll :: (a -> Bool) -> Tree a -> [a]
findAll f = foldMapTree (\x -> [x | f x])

ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing _ = Nothing
ifJust (Just x) f = f x

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree = undefined

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch [] (Node x _ _) = Just x
fetch _ Empty = Nothing
fetch (L:xs) (Node _ ltr _) = fetch xs ltr
fetch (R:xs) (Node _ _ rtr) = fetch xs rtr

mapDirections :: Tree a -> Tree (a, [Direction])
mapDirections = go []
  where
    go _ Empty = Empty
    go xs (Node x ltr rtr) = Node (x, xs) (go (L : xs) ltr) (go (R : xs) rtr)

paths :: Tree a -> [(a, [Direction])]
paths = treeToList . mapDirections
