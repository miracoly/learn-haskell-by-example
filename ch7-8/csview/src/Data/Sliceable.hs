module Data.Sliceable (module Data.Sliceable) where

class Sliceable a where
  slice :: Int -> Int -> a -> a
  slice idx1 idx2 xs = let (_, s, _) = slicePartition idx1 idx2 xs in s
  slicePartition :: Int -> Int -> a -> (a, a, a)

instance Sliceable [a] where
  slicePartition idx1 idx2 xs =
    ( take idx1 xs
    , take (idx2 - idx1) $ drop idx1 xs
    , drop idx2 xs
    )

instance (Sliceable a) => Sliceable (Maybe a) where
  slicePartition idx1 idx2 m =
    case m of
      Nothing -> (Nothing, Nothing, Nothing)
      Just xs ->
        let (hd, s, tl) = slicePartition idx1 idx2 xs
         in (Just hd, Just s, Just tl)

sliceMap :: (Sliceable a, Semigroup a) => Int -> Int -> (a -> a) -> a -> a
sliceMap idx1 idx2 f xs =
  let (hd, sl, tl) = slicePartition idx1 idx2 xs
   in f hd <> f sl <> f tl

sliceDelete :: (Sliceable a, Semigroup a) => Int -> Int -> a -> a
sliceDelete idx1 idx2 xs =
  let (hd, _, tl) = slicePartition idx1 idx2 xs
   in hd <> tl
