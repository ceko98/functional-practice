{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Instances where

import Prelude hiding (reverse)

import Data.Char (isSpace)
import Data.Function (on)

newtype Pointwise a b = Pointwise {getPointwise :: (a, b)}
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (Pointwise a b) where
  (<=) :: Pointwise a b -> Pointwise a b -> Bool
  Pointwise (a, b) <= Pointwise (c, d) = a <= c && b <= d

newtype Lexicographic a b = Lexicographic {getLexicographic :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples and lists
instance (Ord a, Ord b) => Ord (Lexicographic a b) where
  (<=) :: Lexicographic a b -> Lexicographic a b -> Bool
  Lexicographic (a, b) <= Lexicographic (c ,d) = a < c || a == c && b <= d

newtype Fun a b = Fun {getFun :: a -> b}

instance (Semigroup b) => Semigroup (Fun a b) where
  (<>) :: Fun a b -> Fun a b -> Fun a b
  Fun f <> Fun g = Fun (\x -> f x <> g x)

instance (Monoid b) => Monoid (Fun a b) where
  mempty :: Fun a b
  mempty = mempty

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) :: First a -> First a -> First a
  First (Just x) <> _ = First $ Just x
  First Nothing <> x = x

instance Monoid (First a) where
  mempty :: First a
  mempty = mempty

newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) :: Last a -> Last a -> Last a
  Last _ <> Last (Just x) = Last $ Just x
  x <> _ = x 

instance Monoid (Last a) where
  mempty :: Last a
  mempty = mempty

newtype Pair a b = Pair {getPair :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples
instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) :: Pair a b -> Pair a b -> Pair a b
  Pair (a, b) <> Pair (c, d) = Pair (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty :: Pair a b
  mempty = Pair (mempty, mempty)

newtype Dual a = Dual {getDual :: a}
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Dual a) where
  (<>) :: Dual a -> Dual a -> Dual a
  Dual f <> Dual g = Dual $ g <> f

instance Monoid a => Monoid (Dual a) where
  mempty :: Dual a
  mempty = Dual mempty

reverse :: [a] -> [a]
reverse = getDual . foldMap (Dual . (:[]))

data Flux a = Flux
  { sides :: Maybe (a, a)
  , changes :: Int
  }
  deriving (Show, Eq)

flux :: a -> Flux a
flux x = Flux (Just (x, x)) 0

instance (Eq a) => Semigroup (Flux a) where
  (<>) :: Flux a -> Flux a -> Flux a
  Flux Nothing _ <> Flux Nothing _ = Flux Nothing 0
  Flux Nothing _ <> f = f
  f <> Flux Nothing _ = f
  Flux (Just (a, b)) n <> Flux (Just (c, d)) m = Flux (Just (a, d)) (m + n + inc)
    where inc = if b == c then 0 else 1

instance (Eq a) => Monoid (Flux a) where
  mempty :: Flux a
  mempty = Flux Nothing 0
