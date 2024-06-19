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
alter = undefined