module Graph (module Graph) where

import Data.AssocMap qualified as AM
import Data.List qualified as L

type DiGraph a = AM.AssocMap a [a]

addEdges :: (Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges edges graph = L.foldr addEdge graph edges

addEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = AM.alter insertEdge node
  where
    insertEdge Nothing = Just [child]
    insertEdge (Just nodes) = Just (L.nub (child : nodes))

buildDiGraph :: (Eq a) => [(a, [a])] -> DiGraph a
buildDiGraph = L.foldr (uncurry AM.insert) AM.empty

children :: (Eq a) => a -> DiGraph a -> [a]
children = AM.findWithDefault []
