module Csv.Types
  ( DataField (..),
    Column,
    Csv (..),
    unsafeMkCsv,
    mkCsv,
    numberOfRows,
    numberOfColumns,
    appendCsv,
  )
where

import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Sliceable
import qualified Data.Text as T

data Csv = Csv
  { csvHeader :: Maybe [T.Text],
    csvColumns :: [Column]
  }
  deriving (Show)

type Column = [DataField]

data DataField
  = IntValue Int
  | TextValue T.Text
  | NullValue
  deriving (Eq, Show)

instance Semigroup Csv where
  (<>) = appendCsv

instance Monoid Csv where
  mempty = Csv {csvHeader = Nothing, csvColumns = []}

instance Sliceable Csv where
  slicePartition idx1 idx2 Csv {..} =
    let (headerHd, headerSpl, headerTl) = slicePartition idx1 idx2 csvHeader
        (columnHd, columnSpl, columnTl) = slicePartition idx1 idx2 csvColumns
     in (Csv headerHd columnHd, Csv headerSpl columnSpl, Csv headerTl columnTl)

unsafeMkCsv :: Maybe [T.Text] -> [Column] -> Csv
unsafeMkCsv header columns =
  E.either error id $ mkCsv header columns

mkCsv :: Maybe [T.Text] -> [Column] -> Either String Csv
mkCsv mHeader columns
  | not headerSizeCorrect =
      Left "Size of header row does not fit number of columns"
  | not columnSizeCorrect =
      Left "The columns do not have equal sizees"
  | otherwise =
      Right Csv {csvHeader = mHeader, csvColumns = columns}
  where
    headerSizeCorrect =
      M.maybe True (\h -> L.length h == L.length columns) mHeader
    columnSizeCorrect =
      L.length (L.nubBy (\x y -> length x == length y) columns) <= 1

appendCsv :: Csv -> Csv -> Csv
appendCsv a b =
  Csv
    { csvHeader =
        if M.isNothing (csvHeader a) && M.isNothing (csvHeader b)
          then Nothing
          else Just $ header' a ++ header' b,
      csvColumns = appendColumns (csvColumns a) (csvColumns b)
    }
  where
    header' csv =
      M.fromMaybe (L.replicate (numberOfColumns csv) "") (csvHeader csv)
    appendColumns colsA colsB =
      map (++ fillA) colsA ++ map (++ fillB) colsB
      where
        fillA = replicate (numberOfRows b - numberOfRows a) NullValue
        fillB = replicate (numberOfRows a - numberOfRows b) NullValue

numberOfRows :: Csv -> Int
numberOfRows Csv {..} =
  case csvColumns of
    [] -> 0
    h : _ -> length h

numberOfColumns :: Csv -> Int
numberOfColumns Csv {..} = length csvColumns
