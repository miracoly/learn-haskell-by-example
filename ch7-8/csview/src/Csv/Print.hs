module Csv.Print (module Csv.Print) where

import Csv.Parsing
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

printCsv :: Csv -> IO ()
printCsv = mapM_ TIO.putStrLn . toFileContent

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

pretty :: PrettyCsv -> String
pretty = T.unpack . prettyText

prettyText :: PrettyCsv -> T.Text
prettyText PrettyCsv {..} =
  let paddedCols = map padCol pcColumns
      rows = L.transpose paddedCols
      concatedRows = map (L.foldr1 (\a c -> a <> "  " <> c)) rows
   in T.intercalate "\n" concatedRows
  where
    maxColWidth :: [T.Text] -> Int
    maxColWidth = maximum . map T.length
    padCol :: [T.Text] -> [T.Text]
    padCol col = fmap (padField (maxColWidth col)) col
    padField :: Int -> T.Text -> T.Text
    padField max' txt = T.replicate (max' - T.length txt) " " <> txt

writeCsv :: FilePath -> Csv -> IO ()
writeCsv path = TIO.writeFile path . T.intercalate "\n" . toFileContent

toFileContent :: Csv -> [T.Text]
toFileContent Csv {..} =
  let rows = L.map (L.map dataFieldToText) $ L.transpose csvColumns
   in L.map (T.intercalate ",") $ M.maybe rows (: rows) csvHeader
