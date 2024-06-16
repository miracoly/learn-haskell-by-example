module Graph (module Graph) where

import Data.Maybe (isJust)

type Graph k v = [(k, v)]
type DiGraph a = [(a, [a])]

-- | Add a node to the graph
member :: (Eq k) => k -> Graph k v -> Bool
member = (isJust .) . lookup

-- | Like member, but with arguments flipped
hasNode :: (Eq k) => Graph k v -> k -> Bool
hasNode = flip member

-- | Add a node with empty adjacency list to the graph
addNode :: (Eq a) => DiGraph a -> a -> DiGraph a
addNode graph node
  | graph `hasNode` node = graph
  | otherwise = (node, []) : graph

alter :: (Eq k, Eq v) => (Maybe v -> Maybe v) -> k -> Graph k v -> Graph k v
alter f key [] =
  case f Nothing of
    Nothing -> []
    Just value -> [(key, value)]
alter f key ((k, v) : kvs)
  | key == k =
      case f (Just v) of
        Nothing -> kvs
        Just value -> (key, value) : kvs
  | otherwise = (k, v) : alter f key kvs