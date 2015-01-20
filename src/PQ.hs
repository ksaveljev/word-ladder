module PQ ( push
          , pop
          ) where

import Prelude hiding (foldl, foldr, foldl1, foldr1, (.), id)
import Control.Category ((.))
import Control.Arrow (second)
import Data.Monoid
import Data.Foldable
import qualified Data.List as L

newtype Ordered a = Ordered { unOrdered :: [a] } deriving (Show, Eq, Ord)

sort :: (Ord a) => [a] -> Ordered a
sort = Ordered . L.sort

single :: a -> Ordered a
single = Ordered . (:[])

instance Foldable Ordered where
    foldl f s = foldl f s . unOrdered
    foldl1 f = foldl1 f . unOrdered
    foldr f e = foldr f e . unOrdered
    foldr1 f = foldr1 f . unOrdered

instance (Ord a) => Monoid (Ordered a) where
    mempty = Ordered mempty
    mappend l r = Ordered $ unOrdered l `merge` unOrdered r

-- | Merge two ordered lists, preserving duplicates
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] r = r
merge l [] = l
merge ls@(l:lt) rs@(r:rt) = if r < l then r : merge ls rt else l : merge lt rs

newtype Merging m = Merging { unMerging :: [Digit m] } deriving (Show, Eq)
data Digit m = One m | Two m m deriving (Show, Eq)

cons :: (Monoid m) => m -> Merging m -> Merging m
cons a = Merging . cons' a . unMerging
  where
    cons' x [] = [One x]
    cons' x (One r : t) = Two x r : t
    cons' x (Two l r : t) = One x : cons' (l <> r) t

instance Foldable Merging where
    foldl f a = foldl folder a . unMerging
      where
        folder s (One l) = f s l
        folder s (Two l r) = f (f s l) r
    foldr f a = foldr folder a . unMerging
      where
        folder (One l) e = f l e
        folder (Two l r) e = f l (f r e)

instance (Monoid m) => Monoid (Merging m) where
    mempty = Merging mempty
    mappend l r = foldr cons r l

newtype Queue a = Queue { unQueue :: Merging (Ordered a) }

push :: (Ord a) => [a] -> Queue a -> Queue a
push xs = Queue . cons (sort xs) . unQueue

pop :: (Ord a) => Queue a -> Maybe (a, Queue a)
pop = fmap (second Queue) . foldr popper Nothing . unQueue
  where
    popper (Ordered []) t = t
    popper (Ordered xs@(x:xt)) t = case t of
                                     Nothing -> Just (x, cons (Ordered xt) mempty)
                                     Just (y, q) -> Just (if y < x
                                          then (y, cons (Ordered xs) q)
                                          else (x, cons (single y) $ cons (Ordered xt) q))

instance (Ord a) => Monoid (Queue a) where
    mempty = Queue mempty
    mappend l r = Queue $ unQueue l <> unQueue r
