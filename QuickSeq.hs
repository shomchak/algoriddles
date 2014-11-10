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
import Data.Sequence ( ViewL((:<)), ViewR((:>)), (<|), (><))
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

-- | Sort using a function to return the pivot index.
sortWith' :: Ord a => (S.Seq a -> Int) -> [a] -> [a]
sortWith' f = F.toList . sortSeqWith' f . S.fromList

-- | Sort a sequence using a function to return the pivot index.
sortSeqWith' :: Ord a => (S.Seq a -> Int) -> S.Seq a -> S.Seq a
sortSeqWith' f xs = case S.viewl xs of
  S.EmptyL -> S.empty
  _        -> sortSeqWith' f lessers >< S.singleton p >< sortSeqWith' f greaters
    where (lessers, greaters) = partition $ setPivot f xs
          p                   = S.index xs (f xs)

-- | Return the number of comparisons required to sort the list.
sortCount :: Ord a => [a] -> Int
sortCount = sortCountSeqWith (const 0) . S.fromList

-- | Like sortCount using a function to return the index of the pivot.
sortCountWith :: Ord a => (S.Seq a -> Int) -> [a] -> Int
sortCountWith f = sortCountSeqWith f . S.fromList

-- | Prints the input sequence and counts the comparisons.
printSortCountSeqWith :: (Ord a, Show a) => (S.Seq a -> Int) -> S.Seq a -> IO Int
printSortCountSeqWith f xs = case S.viewl xs of
  S.EmptyL -> return 0
  _        -> do
    print xs
    l <- printSortCountSeqWith f lessers
    r <- printSortCountSeqWith f greaters
    return $ l + r + S.length xs - 1
      where (lessers, greaters) = partition $ setPivot f xs

-- | Like sortCountWith for sequences.
sortCountSeqWith :: Ord a => (S.Seq a -> Int) -> S.Seq a -> Int
sortCountSeqWith f xs = case S.viewl xs of
  S.EmptyL -> 0
  _        -> sortCountSeqWith f lessers
              + sortCountSeqWith f greaters
              + S.length xs - 1
    where (lessers, greaters) = partition $ setPivot f xs

-- | A type representing the state of partitioning of a specific partition
-- scheme where the pivot is the first element. For a PS i j S.Seq a,
-- i is the index of the last value <= the pivot (starts at 0, the pivot);
-- j is the index of the last value processed (starts at 0).
-- The hiding of the value constructor in combination with provided methods
-- initP and stepP guarantee that i and j have valid values.
data PartitionState a = PS Int Int (S.Seq a)
                        deriving (Show, Read, Eq)

-- | Produce a the initial state of a partition computation from a sequence.
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

-- | Partition a Sequence using the first element as the pivot.
partition :: Ord a => S.Seq a -> (S.Seq a, S.Seq a)
partition xs = case S.viewl xs of
  S.EmptyL -> (S.empty, S.empty)
  _        -> (lessers, greaters)
    where PS i _ sorted = applyN (S.length xs) stepP (initP xs)
          (_:<rest)     = S.viewl sorted
          (l, greaters) = S.splitAt i rest
          lessers       = rightShift l

-- | Apply a function n times.
applyN :: Int -> (a -> a) -> a -> a
applyN n f x = foldl' (flip ($)) x (replicate n f)

-- | Given a function that takes a sequence to an index, return the sequence
-- with that the elements at that index and 0 swapped.
setPivot :: Ord a => (S.Seq a -> Int) => S.Seq a -> S.Seq a
setPivot f xs = swap 0 (f xs) xs

-- | Swap two elements of a sequence. This function is partial as it is
-- undefined for sequence indices outside the range [0, length-1], but it only
-- called from stepPartition which always uses safe indices.
swap :: Int -> Int -> S.Seq a -> S.Seq a
swap i j xs = S.update j (S.index xs i) (S.update i (S.index xs j) xs)

-- | Shift the sequence one element to the right.
rightShift :: S.Seq a -> S.Seq a
rightShift xs = case S.viewr xs of
  S.EmptyR   -> S.empty
  (beg:>end) -> end <| beg
