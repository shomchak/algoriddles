module Quick (sort) where

-- | Quick sort.
sort :: Ord a => [a] -> [a]
sort []           = []
sort (pivot:rest) = sort lessers ++ [pivot] ++ sort greaters
  where lessers   = filter (<=pivot) rest
        greaters  = filter (>pivot)  rest

-- | A type representing the state of partitioning of a specific partition
-- scheme. For a PartitionState i j [a],
-- i is the index of the last value <= the pivot (starts at 0, the pivot);
-- j is the index of the last value processed (starts at 0).
-- The hiding of the value constructor in combination with provided methods
-- initPartition and stepPartition guarantee that i and j have valid values.
data PartitionState a = PartitionState Int Int [a]
                        deriving (Show, Read, Eq)

-- | Produce a the initial state of a partition computation from a list
-- where the pivot is the first element.
initPartition :: Ord a => [a] -> PartitionState a
initPartition = PartitionState 0 0

-- | Perform one step of the partition computation.
stepPartition :: Ord a => PartitionState a -> PartitionState a
stepPartition (PartitionState i j [])              = PartitionState i j []
stepPartition (PartitionState i j xs@(pivot:rest)) =
  case drop (j+1) xs of
    [] -> PartitionState i j $ swapEdge less ++ greater
      where (less, greater) = splitAt (i+1) xs
    _  -> case xs !! (j+1) > pivot of
      True  -> PartitionState i (j+1) xs
      False -> case i == j of
        True  -> PartitionState (i+1) (j+1) xs
        False -> PartitionState (i+1) (j+1) reordered
          where (less, (g:gs)) = splitAt (i+1) xs
                lefts          = take (j-i-1) gs
                (h:hs)         = drop (j+1) xs
                reordered      = less ++ [h] ++ lefts ++ [g] ++ hs

-- | Partition a list with the pivot as the first element.
partition :: Ord a => [a] -> [a]
partition xs = l
  where PartitionState _ _ l =
          iterate stepPartition (initPartition xs) !! length xs

swapEdge :: [a] -> [a]
swapEdge [] = []
swapEdge (x:xs) = rightShift xs ++ [x]

rightShift :: [a] -> [a]
rightShift [] = []
rightShift xs = let n = length xs - 1 in xs !! n : take n xs
