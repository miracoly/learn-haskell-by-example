module Csv.Print (module Csv.Print) where

import Csv (dataFieldToText)
import Csv.Types
import qualified Data.Either as E
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

unsafeWithSummaries :: PrettyCsv -> [T.Text] -> PrettyCsv
unsafeWithSummaries = (E.either (error "") id .) . withSummaries

withSummaries :: PrettyCsv -> [T.Text] -> Either String PrettyCsv
withSummaries csv@(PrettyCsv {..}) summaries =
  if L.length pcColumns == L.length summaries
    then Right csv {pcSummaries = Just summaries}
    else Left "Summary must have same column length as Csv"

writeCsv :: FilePath -> Csv -> IO ()
writeCsv path = TIO.writeFile path . T.intercalate "\n" . toFileContent

toFileContent :: Csv -> [T.Text]
toFileContent Csv {..} =
  let rows = L.map (L.map dataFieldToText) $ L.transpose csvColumns
   in L.map (T.intercalate ",") $ M.maybe rows (: rows) csvHeader
