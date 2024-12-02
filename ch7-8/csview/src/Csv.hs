module Csv
  ( module Csv.Types,
    module Csv.Parsing,
    module Csv.Print,
    printCsv,
  )
where

import Csv.Parsing
import Csv.Print
import Csv.Types
import qualified Data.Either as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

printCsv :: FilePath -> IO ()
printCsv fp = do
  file <- readFile fp
  let unsafePrettify = prettyText . fromCsv . E.fromRight (error "bla") . parseWithHeader . T.pack
  TIO.putStrLn $ unsafePrettify file
