module Data.AssocMap
  ( AssocMap,
    empty,
    delete,
    insert,
    member,
  )
where

import Graph qualified as G (alter, member)

newtype AssocMap k v = AssocMap [(k, v)]

empty :: AssocMap k v
empty = AssocMap []

delete :: (Eq k) => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: (Eq k) => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

-- | Add a node to the Associative Map
member :: (Eq k) => k -> AssocMap k v -> Bool
member key (AssocMap am) = G.member key am

alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter f key (AssocMap am) = AssocMap $ G.alter f key am