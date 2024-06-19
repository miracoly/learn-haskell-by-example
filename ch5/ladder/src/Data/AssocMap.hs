module Data.AssocMap
  ( AssocMap,
    empty,
    delete,
    insert,
    member,
    alter,
    lookup,
    findWithDefault
  )
where

import Prelude hiding (lookup)
import Prelude qualified as P
import Data.Maybe (fromMaybe, isJust)

newtype AssocMap k v = AssocMap [(k, v)]
  deriving (Show, Eq)

empty :: AssocMap k v
empty = AssocMap []

delete :: (Eq k) => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: (Eq k) => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

member :: (Eq k) => k -> AssocMap k v -> Bool
member key (AssocMap am) = _member key am

lookup :: (Eq k) => k -> AssocMap k v -> Maybe v
lookup key (AssocMap am) = P.lookup key am

findWithDefault :: (Eq k) => v -> k -> AssocMap k v -> v
findWithDefault def key = fromMaybe def . lookup key

alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter f key (AssocMap am) = AssocMap $ _alter f key am

type Graph k v = [(k, v)]
type DiGraph a = [(a, [a])]

-- | Add a node to the graph
_member :: (Eq k) => k -> Graph k v -> Bool
_member = (isJust .) . P.lookup

-- | Like member, but with arguments flipped
_hasNode :: (Eq k) => Graph k v -> k -> Bool
_hasNode = flip _member

-- | Add a node with empty adjacency list to the graph
_addNode :: (Eq a) => DiGraph a -> a -> DiGraph a
_addNode graph node
  | graph `_hasNode` node = graph
  | otherwise = (node, []) : graph

_alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> Graph k v -> Graph k v
_alter f key [] =
  case f Nothing of
    Nothing -> []
    Just value -> [(key, value)]
_alter f key ((k, v) : kvs)
  | key == k =
      case f (Just v) of
        Nothing -> kvs
        Just value -> (key, value) : kvs
  | otherwise = (k, v) : _alter f key kvs