module PermutationMap (module PermutationMap) where

import Data.AssocMap qualified as AM
import Data.List qualified as L

type PermutationMap = AM.AssocMap String [String]

empty :: PermutationMap
empty = AM.empty

member :: String -> PermutationMap -> Bool
member key = AM.member (L.sort key)

alter ::
  (Maybe [String] -> Maybe [String]) ->
  String ->
  PermutationMap ->
  PermutationMap
alter f key = AM.alter f $ L.sort key

delete :: String -> PermutationMap -> PermutationMap
delete key = AM.delete (L.sort key) 

insert :: String -> [String] -> PermutationMap -> PermutationMap
insert key = AM.insert (L.sort key)

lookup :: String -> PermutationMap -> Maybe [String]
lookup key = AM.lookup (L.sort key)

createPermutationMap :: [String] -> PermutationMap
createPermutationMap = foldr insertPermutation empty
  where
    insertPermutation :: String -> PermutationMap -> PermutationMap
    insertPermutation word = alter (insertList word) word
    insertList :: a -> Maybe [a] -> Maybe [a]
    insertList w Nothing = Just [w]
    insertList w (Just ws) = Just (w : ws)