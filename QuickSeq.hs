{-# OPTIONS_GHC -Wall #-}

-- Implementations of quick sort using sequences under the hood.

module QuickSeq ( sort
                , sortSeq
                , sort'
                , sortWith'
                , sortCount
                , sortCountWith
                ) where

import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)), (><))
import qualified Data.Foldable as F
import Data.List (foldl')

-- | Quick sort. Converts a list to a sequence to perform the sorting.
sort :: Ord a => [a] -> [a]
sort = F.toList . sortSeq . S.fromList

-- | Sort a sequence using quick sort.
sortSeq :: Ord a => S.Seq a -> S.Seq a
sortSeq xs = case S.viewl xs of
  S.EmptyL      -> S.empty
  (pivot:<rest) -> sortSeq lessers >< S.singleton pivot >< sortSeq greaters
    where lessers  = S.filter (<=pivot) rest
          greaters = S.filter (>pivot)  rest

-- | Quick sort using the specific partition scheme below.
sort' :: Ord a => [a] -> [a]
sort' = sortWith' $ const 0

sortWith' :: Ord a => (S.Seq a -> Int) -> [a] -> [a]
sortWith' f = F.toList . sortSeqWith' f . S.fromList

-- | Sort a sequence using quick sort.
sortSeqWith' :: Ord a => (S.Seq a -> Int) -> S.Seq a -> S.Seq a
sortSeqWith' f xs = case S.viewl xs of
  S.EmptyL -> S.empty
  _        -> sortSeqWith' f lessers >< S.singleton p >< sortSeqWith' f greaters
    where (lessers, greaters) = partitionWith f xs
          p                   = S.index xs (f xs)

-- | Return the number of comparisons required to sort the list.
sortCount :: Ord a => [a] -> Int
sortCount = sortCountSeqWith (const 0) . S.fromList

-- | Return the number of comparisons required to sort the list.
sortCountWith :: Ord a => (S.Seq a -> Int) -> [a] -> Int
sortCountWith f = sortCountSeqWith f . S.fromList

-- | Return the number of comparisons required to sort the sequence.
sortCountSeqWith :: Ord a => (S.Seq a -> Int) -> S.Seq a -> Int
sortCountSeqWith f xs = case S.viewl xs of
  S.EmptyL -> 0
  _        -> sortCountSeqWith f lessers
              + sortCountSeqWith f greaters
              + S.length xs - 1
    where (lessers, greaters) = partitionWith f xs

-- | A type representing the state of partitioning of a specific partition
-- scheme where the pivot is the first element. For a PS i j S.Seq a,
-- i is the index of the last value <= the pivot (starts at 0, the pivot);
-- j is the index of the last value processed (starts at 0).
-- The hiding of the value constructor in combination with provided methods
-- initP and stepP guarantee that i and j have valid values.
data PartitionState a = PS Int Int (S.Seq a)
                        deriving (Show, Read, Eq)

-- | Produce a the initial state of a partition computation from a list.
initP :: Ord a => S.Seq a -> PartitionState a
initP = PS 0 0

-- | Perform one step of the partition computation.
stepP :: Ord a => PartitionState a -> PartitionState a
stepP (PS i j xs) = case S.viewl xs of
  S.EmptyL -> PS i j xs  -- Do nothing for an empty sequence
  pivot:<_ -> case S.length xs == (j+1) of
    True  -> PS i j xs   -- Do nothing if we have processed all elements
    False -> case S.index xs (j+1) > pivot of
      True  -> PS i (j+1) xs
      False -> PS (i+1) (j+1) $ swap (i+1) (j+1) xs

-- | Partition a Sequence using a function to return the index of the pivot.
partitionWith :: Ord a => (S.Seq a -> Int) -> S.Seq a -> (S.Seq a, S.Seq a)
partitionWith f xs = case S.viewl xs of
  S.EmptyL -> (S.empty, S.empty)
  _        -> S.splitAt i rest
    where PS i _ sorted = applyN (S.length xs) stepP (initP (setPivot f xs))
          (_:<rest)     = S.viewl sorted

-- | Apply a function n times.
applyN :: Int -> (a -> a) -> a -> a
applyN n f x = foldl' (flip ($)) x (replicate n f)

-- | Given a function that takes a sequence to an index, return the sequence
-- with that index and 0 swapped.
setPivot :: Ord a => (S.Seq a -> Int) => S.Seq a -> S.Seq a
setPivot f xs = swap 0 (f xs) xs

-- | Swap two elements of a list. This function is partial as it is undefined
-- for list indices outside the range [0, length-1], but it only called from
-- stepPartition which always uses safe indices.
swap :: Int -> Int -> S.Seq a -> S.Seq a
swap i j xs = S.update j (S.index xs i) (S.update i (S.index xs j) xs)

