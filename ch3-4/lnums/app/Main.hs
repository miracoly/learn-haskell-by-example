module Main (main) where

import Data.Maybe
import Lib (PadMode (..), numberAllLines, prettyNumberedLines)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  cliArgs <- getArgs
  let (mFilePath, _) = parseArguments cliArgs
  maybe
    (printHelpText "Missing filename")
    ( \filePath -> do
        fileLines <- readLines filePath
        let numbered = numberAllLines fileLines
            prettyNumbered = prettyNumberedLines PadLeft numbered
        mapM_ putStrLn prettyNumbered
    )
    mFilePath

readLines :: FilePath -> IO [String]
readLines filePath = do
  contents <- readFile filePath
  return (lines contents)

data LineNumberOption
  = ReverseNumbering
  | SkipEmptyLines
  | LeftAlign
  deriving (Eq)

parseArguments :: [String] -> (Maybe FilePath, [LineNumberOption])
parseArguments args = case reverse args of
  [] -> (Nothing, [])
  (filename : options) ->
    ( Just filename
    , mapMaybe lnOptionFromString options
    )

lnOptionFromString :: String -> Maybe LineNumberOption
lnOptionFromString "--reverse" = Just ReverseNumbering
lnOptionFromString "--skip-empty" = Just SkipEmptyLines
lnOptionFromString "--left-align" = Just LeftAlign
lnOptionFromString _ = Nothing

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <options> <filename>")
  putStrLn "\n"
  putStrLn " Options:"
  putStrLn "   --reverse      - Reverse the numbering"
  putStrLn "   --skip-empty   - Skip numbering empty lines"
  putStrLn "   --left-align   - Use left-aligned line numbers"
