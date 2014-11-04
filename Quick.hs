module Quick (sort) where

-- | Quick sort.
sort :: Ord a => [a] -> [a]
sort []           = []
sort (pivot:rest) = sort lessers ++ [pivot] ++ sort greaters
  where lessers   = filter (<=pivot) rest
        greaters  = filter (>pivot)  rest
