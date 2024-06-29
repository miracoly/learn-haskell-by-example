{-# LANGUAGE ScopedTypeVariables #-}

module Graph (module Graph) where

import Data.HashMap.Lazy qualified as M
import Data.Hashable (Hashable)
import Data.List qualified as L

type DiGraph a = M.HashMap a [a]

addEdges :: (Hashable a, Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges edges graph = L.foldr addEdge graph edges

addEdge :: (Hashable a, Eq a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
  where
    insertEdge Nothing = Just [child]
    insertEdge (Just nodes) = Just (L.nub (child : nodes))

buildDiGraph :: (Hashable a, Eq a) => [(a, [a])] -> DiGraph a
buildDiGraph = L.foldr (uncurry M.insert) M.empty

children :: (Hashable a, Eq a) => a -> DiGraph a -> [a]
children = M.findWithDefault []

data SearchResult a = Unsuccessful | Successful (DiGraph a)

type SearchState a = ([a], DiGraph a, DiGraph a)

bfsSearch :: forall a. (Hashable a, Eq a) => DiGraph a -> a -> a -> Maybe [a]
bfsSearch graph start end
  | start == end = Just [start]
  | otherwise =
      case bfsSearch' ([start], graph, M.empty) of
        Successful preds -> Just (findSolution preds)
        Unsuccessful -> Nothing
  where
    findSolution :: DiGraph a -> [a]
    findSolution g = L.reverse (go end)
      where
        go x =
          case children x g of
            [] -> [x]
            (v : _) -> x : go v

    addMultiplePredecessors :: [(a, [a])] -> DiGraph a -> DiGraph a
    addMultiplePredecessors [] g = g
    addMultiplePredecessors ((n, ch) : xs) g =
      addMultiplePredecessors xs (go n ch g)
      where
        go _ [] g' = g'
        go n' (x : xs') g' = go n' xs' (addEdge (x, n') g')

    bfsSearch' :: SearchState a -> SearchResult a
    bfsSearch' ([], _, _) = Unsuccessful
    bfsSearch' (frontier, g, preds) =
      let g' = deleteNodes frontier g
          ch =
            L.map
              (\n -> (n, L.filter (`M.member` g') (children n g)))
              frontier
          frontier' = L.concatMap snd ch
          preds' = addMultiplePredecessors ch preds
       in if end `L.elem` frontier'
            then Successful preds'
            else bfsSearch' (frontier', g', preds')

    deleteNodes :: [a] -> DiGraph a -> DiGraph a
    deleteNodes xs g = foldr M.delete g xs