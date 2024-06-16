module Data.AssocMap
  ( AssocMap,
    empty,
    delete,
    insert,
    member,
    lookup,
    findWithDefault
  )
where

import Graph qualified as G (alter, member)
import Prelude hiding (lookup)
import Prelude qualified as P
import Data.Maybe (fromMaybe)

newtype AssocMap k v = AssocMap [(k, v)]
  deriving (Show, Eq)

empty :: AssocMap k v
empty = AssocMap []

delete :: (Eq k) => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: (Eq k) => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

member :: (Eq k) => k -> AssocMap k v -> Bool
member key (AssocMap am) = G.member key am

lookup :: (Eq k) => k -> AssocMap k v -> Maybe v
lookup key (AssocMap am) = P.lookup key am

findWithDefault :: (Eq k) => v -> k -> AssocMap k v -> v
findWithDefault def key = fromMaybe def . lookup key

alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter f key (AssocMap am) = AssocMap $ G.alter f key am