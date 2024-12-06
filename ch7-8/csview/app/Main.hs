module Main (main) where

import Csv (printCsv)
import qualified Data.Text as T
import System.Environment (getArgs)

main :: IO ()
main = printCsv "csvs/cities.csv"

getArguments :: IO [T.Text]
getArguments = fmap (map T.pack) getArgs

getBool :: T.Text -> IO Bool
getBool = undefined

getChar :: T.Text -> IO (Maybe Char)
getChar = undefined

getText :: T.Text -> IO (Maybe T.Text)
getText = undefined

getInterval :: T.Text -> IO (Maybe (Int, Int))
getInterval = undefined
