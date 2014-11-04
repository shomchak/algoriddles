module Quick (sort) where
import Data.List (splitAt)

-- | Quick sort.
sort :: Ord a => [a] -> [a]
sort []  = []
sort xs  = sort lessers ++ [pivot] ++ sort greaters
  where p_index                 = (length xs `quot` 2)
        (lefts, (pivot:rights)) = splitAt p_index xs
        rest                    = lefts ++ rights
        lessers                 = filter (<=pivot) rest
        greaters                = filter (>pivot) rest
