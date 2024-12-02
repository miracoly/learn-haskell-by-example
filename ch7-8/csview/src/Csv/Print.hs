module Csv.Print (module Csv.Print) where

import Csv (dataFieldToText)
import Csv.Types
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data PrettyCsv = PrettyCsv
  { pcHeader :: Maybe [T.Text],
    pcColumns :: [[T.Text]],
    pcSummaries :: Maybe [T.Text]
  }
  deriving (Eq, Show)

fromCsv :: Csv -> PrettyCsv
fromCsv Csv {..} =
  PrettyCsv
    { pcHeader = csvHeader,
      pcColumns = L.map (L.map dataFieldToText) csvColumns,
      pcSummaries = Nothing
    }

writeCsv :: FilePath -> Csv -> IO ()
writeCsv path = TIO.writeFile path . T.intercalate "\n" . toFileContent

toFileContent :: Csv -> [T.Text]
toFileContent Csv {..} =
  let rows = L.map (L.map dataFieldToText) $ L.transpose csvColumns
   in L.map (T.intercalate ",") $ M.maybe rows (: rows) csvHeader
