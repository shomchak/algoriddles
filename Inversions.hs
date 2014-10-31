module Inversions where
import Data.List (splitAt)
import Data.Sequence ( (|>), (<|), (><) )
import qualified Data.Sequence as DS

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





-- sortCount' :: O rd a => [a] -> Int -> ([a], Int)
-- sortCount' [] n = ([], n)
-- sortCount' (x:[]) n = ([x], n)
-- sortCount' xs =

-- | Return the number of inversions in a list.
inversions' :: Ord a => [a] -> Int
inversions' = snd . sortCount' . DS.fromList

-- | Sort a list using merge sort, and count the inversions.
sortCount' :: Ord a => DS.Seq a -> (DS.Seq a, Int)
sortCount' DS.empty     = (DS.empty,  0)
sortCount' (x <| DS.empty) = (DS.singleton x, 0)
sortCount' xs     = (l,   split_count + left_count + right_count)
  where (left,    right)       = DS.splitAt (DS.length xs `quot` 2) xs
        (left_l,  left_count)  = sortCount' left
        (right_l, right_count) = sortCount' right
        (l,       split_count) = mergeCount' left_l right_l

-- | Merge two sorted lists, count the split inversions.
mergeCount' :: Ord a => DS.Seq a -> DS.Seq a -> (DS.Seq a, Int)
mergeCount' = mergeCountIter DS.empty 0

mergeCountIter :: Ord a => DS.Seq a -> Int -> DS.Seq a -> DS.Seq a -> (DS.Seq a, Int)
mergeCountIter result n xs DS.empty = (result >< xs, n)
mergeCountIter result n DS.empty ys = (result >< ys, n)
mergeCountIter result n left right
  | l <= r    = mergeCountIter (result |> l) n (DS.tail left) right
  | otherwise = mergeCountIter (result |> r) (n + length left) left (DS.tail right)
    where l = DS.head left
          r = DS.head right
