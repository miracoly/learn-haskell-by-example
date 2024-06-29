module Ladder (module Ladder) where

import Data.HashMap.Lazy qualified as M
import Data.List qualified as L
import Graph qualified as G
import PermutationMap qualified as PM

type Dictionary = [String]

readDictionary :: FilePath -> IO Dictionary
readDictionary filepath = do
  content <- readFile filepath
  let _lines = L.lines content
      _words = L.map (L.filter (`L.elem` ['a' .. 'z'])) _lines
  return (L.nub _words)

mkLadderGraph :: Dictionary -> G.DiGraph String
mkLadderGraph dict = G.buildDiGraph nodes
  where
    _map = PM.createPermutationMap dict
    nodes = L.map (\w -> (w, computeCandidates _map w)) dict

computeCandidates :: PM.PermutationMap -> String -> [String]
computeCandidates _map word =
  let candidates = modified <> removed <> added <> [word]
      uniques = L.nub [L.sort w | w <- candidates]
      perms = L.concatMap (\x -> M.findWithDefault [] x _map) uniques
   in L.delete word perms
  where
    added = [x : word | x <- ['a' .. 'z']]
    removed = [L.delete x word | x <- word]
    modified = [x : L.delete y word | x <- ['a' .. 'z'], y <- word, x /= y]

ladderSolve :: Dictionary -> String -> String -> Maybe [String]
ladderSolve dict start end =
  let g = mkLadderGraph dict
   in G.bfsSearch g start end