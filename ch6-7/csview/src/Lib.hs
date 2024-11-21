module Lib
  ( module Lib,
  )
where

import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Maybe as M
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
