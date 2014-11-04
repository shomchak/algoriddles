module Quick (sort) where
import Data.List (splitAt)

-- | Quick sort.
sort :: Ord a => [a] -> [a]
sort []            = []
sort (pivot:rest)  = sort lessers ++ [pivot] ++ sort greaters
  where lessers  = filter (<=pivot) rest
        greaters = filter (>pivot)  rest
