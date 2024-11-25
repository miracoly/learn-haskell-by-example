module Data.Sliceable (module Data.Sliceable) where

class Sliceable a where
  slice :: Int -> Int -> a -> a
  slice idx1 idx2 xs = let (_, s, _) = slicePartition idx1 idx2 xs in s
  slicePartition :: Int -> Int -> a -> (a, a, a)
