module Merge where
import Data.List (splitAt)

sort :: Ord a => [a] -> [a]
sort []     = []
sort (x:[]) = [x]
sort xs     = merge (sort left) (sort right)
  where (left, right) = splitAt (length xs `quot` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] []   = []
merge xs []  = xs
merge [] ys  = ys
merge left right
  | l <= r    = l : (merge (tail left) right)
  | otherwise = r : (merge left $ tail right)
    where l = head left
          r = head right
