module Csv.Operations (module Csv.Operations) where

import Csv.Parsing (dataFieldToText)
import Csv.Types (Csv (..), DataField (NullValue))
import qualified Data.List as L
import qualified Data.Text as T

foldCsv :: (DataField -> b -> b) -> b -> Csv -> [b]
foldCsv f z (Csv {csvColumns}) = map (foldr f z) csvColumns

filterCsv :: (DataField -> Bool) -> Csv -> Csv
filterCsv p csv@(Csv {csvColumns}) =
  let rows = L.transpose csvColumns
      filtered = L.filter (any p) rows
   in csv {csvColumns = L.transpose filtered}

countNonEmpty :: Csv -> [Int]
countNonEmpty = foldCsv f 0
  where
    f NullValue acc = acc
    f _ acc = acc + 1

countOccurences :: DataField -> Csv -> [Int]
countOccurences df = foldCsv (\x acc -> if x == df then acc + 1 else acc) 0

searchText :: T.Text -> Csv -> Csv
searchText t = filterCsv (\f -> dataFieldToText f `contains` t)
  where
    contains = flip T.isInfixOf
