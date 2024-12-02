module Main (main) where
import Csv (printCsv)

main :: IO ()
main = printCsv "csvs/cities.csv"
