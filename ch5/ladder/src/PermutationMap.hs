module PermutationMap (module PermutationMap) where

import Data.HashMap.Lazy qualified as M
import Data.List qualified as L

type PermutationMap = M.HashMap String [String]

empty :: PermutationMap
empty = M.empty

member :: String -> PermutationMap -> Bool
member key = M.member (L.sort key)

alter ::
  (Maybe [String] -> Maybe [String]) ->
  String ->
  PermutationMap ->
  PermutationMap
alter f key = M.alter f $ L.sort key

delete :: String -> PermutationMap -> PermutationMap
delete key = M.delete (L.sort key) 

insert :: String -> [String] -> PermutationMap -> PermutationMap
insert key = M.insert (L.sort key)

lookup :: String -> PermutationMap -> Maybe [String]
lookup key = M.lookup (L.sort key)

createPermutationMap :: [String] -> PermutationMap
createPermutationMap = foldr insertPermutation empty
  where
    insertPermutation :: String -> PermutationMap -> PermutationMap
    insertPermutation word = alter (insertList word) word
    insertList :: a -> Maybe [a] -> Maybe [a]
    insertList w Nothing = Just [w]
    insertList w (Just ws) = Just (w : ws)