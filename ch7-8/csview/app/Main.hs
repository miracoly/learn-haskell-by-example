module Main (main) where

import qualified Csv
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Util.Arguments as Args

parseInFile :: T.Text -> IO (Either String Csv.Csv)
parseInFile key = do
  mInFile <- Args.getText key
  mFieldSep <- Args.getChar "field-separator"
  hasHeader <- Args.getBool "with-header"
  let separators =
        Csv.defaultSeparators
          { Csv.sepFieldSeparator =
              M.fromMaybe
                (Csv.sepFieldSeparator Csv.defaultSeparators)
                mFieldSep
          }
      headerOpt =
        if hasHeader
          then Csv.WithHeader
          else Csv.WithoutHeader
      parseOpts =
        Csv.CsvParseOptions
          { Csv.cpoSeparators = separators
          , Csv.cpoHeaderOptions = headerOpt
          }
  case mInFile of
    Just inFile -> do
      contents <- TIO.readFile $ T.unpack inFile
      return $ Csv.parseCsv parseOpts contents
    _ -> return $ Left "argument not set"

main :: IO ()
main = Csv.printCsv "csvs/cities.csv"
