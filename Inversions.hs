module Inversions (inversions) where
import Data.List (splitAt)

-- | Return the number of inversions in a list.
inversions :: Ord a => [a] -> Int
inversions = snd . sortCount

-- | Sort a list using merge sort, and count the inversions.
sortCount :: Ord a => [a] -> ([a], Int)
sortCount []     = ([],  0)
sortCount (x:[]) = ([x], 0)
sortCount xs     = (l,   split_count + left_count + right_count)
  where (left,    right)       = splitAt (length xs `quot` 2) xs
        (left_l,  left_count)  = sortCount left
        (right_l, right_count) = sortCount right
        (l,       split_count) = mergeCount left_l right_l

-- | Merge two sorted lists, count the split inversions.
mergeCount :: Ord a => [a] -> [a] -> ([a], Int)
mergeCount xs [] = (xs, 0)
mergeCount [] ys = (ys, 0)
mergeCount left right
  | l <= r       = (l:left_rest,  left_count)
  | otherwise    = (r:right_rest, right_count + length left)
    where l                         = head left
          r                         = head right
          (left_rest,  left_count)  = mergeCount (tail left) right
          (right_rest, right_count) = mergeCount left $ tail right
