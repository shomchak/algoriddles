-- Implementations of quick sort using sequences under the hood.

module QuickSeq (sort, ) where
import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)), (><))
import Data.Foldable (toList)

-- | Quick sort. Converts a list to a sequence to perform the sorting.
sort :: Ord a => [a] -> [a]
sort = toList . sortSeq . S.viewl . S.fromList

-- | Sort a sequence using quick sort. Takes a ViewL for pattern matching.
sortSeq :: Ord a => S.ViewL a -> S.Seq a
sortSeq S.EmptyL    = S.empty
sortSeq (p:<rest) = sortSeq lessers >< S.singleton p >< sortSeq greaters
  where lessers   = S.viewl $ S.filter (<=p) rest
        greaters  = S.viewl $ S.filter (>p)  rest

---- | Quick sort using the specific partition scheme below.
--sort' :: Ord a => [a] -> [a]
--sort' []           = []
--sort' xs@(pivot:_) = sort' lessers ++ [pivot] ++ sort' greaters
--  where (lessers, greaters) = partition xs

---- | Sort a sequence using quick sort. Takes a ViewL for pattern matching.
--sortSeq' :: Ord a => S.ViewL a -> S.Seq a
--sortSeq' S.EmptyL    = S.empty
--sortSeq' (p:<rest) = sortSeq' lessers >< S.singleton p >< sortSeq' greaters
--  where lessers    = S.viewl $ S.filter (<=p) rest
--        greaters   = S.viewl $ S.filter (>p)  rest

-- | A type representing the state of partitioning of a specific partition
-- scheme where the pivot is the first element. For a PartitionState i j S.Seq a,
-- i is the index of the last value <= the pivot (starts at 0, the pivot);
-- j is the index of the last value processed (starts at 0).
-- The hiding of the value constructor in combination with provided methods
-- initPartition and stepPartition guarantee that i and j have valid values.
data PartitionState a = PartitionState Int Int (S.Seq a)
                        deriving (Show, Read, Eq)

-- | Produce a the initial state of a partition computation from a list.
initPartition :: Ord a => S.Seq a -> PartitionState a
initPartition = PartitionState 0 0

-- | Perform one step of the partition computation.
stepPartition :: Ord a => PartitionState a -> PartitionState a
stepPartition (PartitionState i j xs) =
  case S.viewl xs of
    S.EmptyL -> PartitionState i j xs
    pivot:<_ -> case S.length xs == (j+1) of
      True  -> PartitionState i j xs
      False -> case S.index xs (j+1) > pivot of
        True  -> PartitionState i (j+1) xs
        False -> PartitionState (i+1) (j+1) $
          S.update j' (S.index xs i') (S.update i' (S.index xs j') xs)
            where i' = i+1
                  j' = j+1

-- | Partition a list with the pivot as the first element.
partition :: Ord a => S.Seq a -> (S.Seq a, S.Seq a)
partition xs = S.splitAt i rest
  where PartitionState i _ sorted =
          iterate stepPartition (initPartition xs) !! S.length xs
        (_:<rest)                 = S.viewl sorted

---- | Swap two elements of a list. This function is partial as it is undefined
---- for list indices outside the range [0, length-1], but it only called from
---- stepPartition which always uses safe indices.
--swap :: Int -> Int -> [a] -> [a]
--swap i j xs = zipWith new_el [0..length xs - 1] xs
--  where new_el k x | k == i    = xs !! j
--                   | k == j    = xs !! i
--                   | otherwise = x
