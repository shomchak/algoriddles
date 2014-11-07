module Quick (sort, sort') where

-- | Quick sort.
sort :: Ord a => [a] -> [a]
sort []           = []
sort (pivot:rest) = sort lessers ++ [pivot] ++ sort greaters
  where lessers   = filter (<=pivot) rest
        greaters  = filter (>pivot)  rest

-- | Quick sort using the specific partition scheme below.
sort' :: Ord a => [a] -> [a]
sort' []           = []
sort' xs@(pivot:_) = sort' lessers ++ [pivot] ++ sort greaters
  where (lessers, greaters, _) = partition xs

sortCount :: Ord a => [a] -> Int
sortCount [] = 0
sortCount xs@(pivot:_) = sortCount lessers + sortCount greaters + count
  where (lessers, greaters, count) = partition xs


-- | A type representing the state of partitioning of a specific partition
-- scheme where the pivot is the first element. For a PartitionState i j [a],
-- i is the index of the last value <= the pivot (starts at 0, the pivot);
-- j is the index of the last value processed (starts at 0).
-- The hiding of the value constructor in combination with provided methods
-- initPartition and stepPartition guarantee that i and j have valid values.
data PartitionState a = PartitionState Int Int [a]
                        deriving (Show, Read, Eq)

-- | Produce a the initial state of a partition computation from a list.
initPartition :: Ord a => [a] -> PartitionState a
initPartition = PartitionState 0 0

-- | Perform one step of the partition computation.
stepPartition :: Ord a => PartitionState a -> PartitionState a
stepPartition (PartitionState i j [])              = PartitionState i j []
stepPartition (PartitionState i j xs@(pivot:_)) =
  case drop (j+1) xs of
    [] -> PartitionState i j xs
    _  -> case xs !! (j+1) > pivot of
      True  -> PartitionState i (j+1) xs
      False -> PartitionState (i+1) (j+1) $ swap (i+1) (j+1) xs

-- | Partition a list with the pivot as the first element.
partition :: Ord a => [a] -> ([a], [a], Int)
partition [] = ([], [], 0)
partition xs = (left, right, count)
  where PartitionState i _ (_:rest) =
          iterate stepPartition (initPartition xs) !! (count + 1)
        (left, right)               = splitAt i rest
        count                       = length xs - 1

-- partitionCount :: Ord a => [a] -> ([a], [a], Int)
-- partitionCount xs = (left, right, count)

-- | Swap two elements of a list. This function is partial as it is undefined
-- for list indices outside the range [0, length-1], but it only called from
-- stepPartition which always uses safe indices.
swap :: Int -> Int -> [a] -> [a]
swap i j xs = zipWith new_el [0..length xs - 1] xs
  where new_el k x | k == i    = xs !! j
                   | k == j    = xs !! i
                   | otherwise = x

