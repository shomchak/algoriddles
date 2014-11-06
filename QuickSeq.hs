-- Implementations of quick sort using sequences under the hood.

module QuickSeq ( sort
                , sortSeq
                , sort'
                , sortSeq'
                ) where

import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)), (><))
import qualified Data.Foldable as F

-- | Quick sort. Converts a list to a sequence to perform the sorting.
sort :: Ord a => [a] -> [a]
sort = F.toList . sortSeq . S.fromList

-- | Sort a sequence using quick sort. Takes a ViewL for pattern matching.
sortSeq :: Ord a => S.Seq a -> S.Seq a
sortSeq xs = case S.viewl xs of
  S.EmptyL      -> S.empty
  (pivot:<rest) -> sortSeq lessers >< S.singleton pivot >< sortSeq greaters
    where lessers  = S.filter (<=pivot) rest
          greaters = S.filter (>pivot)  rest

-- | Quick sort using the specific partition scheme below.
sort' :: Ord a => [a] -> [a]
sort' = F.toList . sortSeq' . S.fromList

-- | Sort a sequence using quick sort. Takes a ViewL for pattern matching.
sortSeq' :: Ord a => S.Seq a -> S.Seq a
sortSeq' xs = case S.viewl xs of
  S.EmptyL  -> S.empty
  (p:<_)    -> sortSeq' lessers >< S.singleton p >< sortSeq' greaters
    where (lessers, greaters) = partition xs


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
  S.EmptyL -> PS i j xs
  pivot:<_ -> case S.length xs == (j+1) of
    True  -> PS i j xs
    False -> case S.index xs (j+1) > pivot of
      True  -> PS i (j+1) xs
      False -> PS (i+1) (j+1) $
        S.update j' (S.index xs i') (S.update i' (S.index xs j') xs)
          where i' = i+1
                j' = j+1

-- | Partition a Sequence with the pivot as the first element.
partition :: Ord a => S.Seq a -> (S.Seq a, S.Seq a)
partition xs = case S.viewl xs of
  S.EmptyL -> (S.empty, S.empty)
  _        -> S.splitAt i rest
    where PS i _ sorted = iterate stepP (initP xs) !! S.length xs
          (_:<rest)     = S.viewl sorted
