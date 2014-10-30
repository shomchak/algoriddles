module Inversions (inversions) where
import Data.List (splitAt)

-- | Return the number of inversions in a list.
inversions :: Ord a => [a] -> Int
inversions = snd . sortCount

-- | Sort a list using merge sort, and count the inversions.
sortCount :: Ord a => [a] -> ([a], Int)
sortCount []     = ([], 0)
sortCount (x:[]) = ([x], 0)
sortCount xs     = (l, split_count + count_left + count_right)
  where (left, right)          = splitAt (length xs `quot` 2) xs
        (l_left, count_left)   = sortCount left
        (l_right, count_right) = sortCount right
        (l, split_count)       = mergeCount l_left l_right

-- | Merge two sorted lists, counting the inversions.
mergeCount :: Ord a => [a] -> [a] -> ([a], Int)
mergeCount xs []   = (xs, 0)
mergeCount [] ys   = (ys, 0)
mergeCount left right
  | l <= r    = (l:rest_left, count_left)
  | otherwise = (r:rest_right, (length left) + count_right)
    where l                         = head left
          r                         = head right
          (rest_left, count_left)   = mergeCount (tail left) right
          (rest_right, count_right) = (mergeCount left $ tail right)
