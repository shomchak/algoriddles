module Graph where

-- | Unweighted, undirected, no parallel edges Graph. Represented as an
-- adjacency list.
data UnweightedGraph a = UGraph [[a]]

fromList :: Eq a => [[a]] -> UnweightedGraph a
fromList = UGraph

toList :: Eq a => UnweightedGraph a -> [[a]]
toList (UGraph nodes) = nodes
