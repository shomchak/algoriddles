module Graph where

-- | Unweighted, undirected, no parallel edges Graph. Represented as an
-- adjacency list.
data UnweightedGraph a = UGraph [[a]]
