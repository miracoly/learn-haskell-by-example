module Lib (module Lib) where

import Data.Char (isPrint, isSeparator)

type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]

numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (not . isEmpty) (const True)

numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines (not . isEmpty) (not . isEmpty)

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNum lns =
  let
    go :: Int -> [String] -> NumberedLines
    go _ [] = []
    go counter (s : ss) =
      let
        mNumbering = if shouldNum s then Just counter else Nothing
        newCounter = if shouldIncr s then counter + 1 else counter
       in
        (mNumbering, s) : go newCounter ss
   in
    go 1 lns

isEmpty :: String -> Bool
isEmpty str =
  null str || all (\s -> not (isPrint s) || isSeparator s) str

data PadMode = PadLeft | PadRight | PadCenter

prettyNumberedLines :: PadMode -> NumberedLines -> [String]
prettyNumberedLines mode lnsNums = 
  let
    (numbers, lns) = unzip lnsNums
    numberStrs = map (maybe "" show) numbers
    maxLen = maximum (map length numberStrs)
    paddedNums = map (pad mode maxLen) numberStrs
  in zipWith (\n l -> n ++ " " ++ l) paddedNums lns

padLeft :: Int -> String -> String
padLeft = pad PadLeft

padRight :: Int -> String -> String
padRight = pad PadRight

padCenter :: Int -> String -> String
padCenter = pad PadCenter

pad :: PadMode -> Int -> String -> String
pad padMode desiredLen str =
  let
    diff = desiredLen - length str
    padding = replicate diff ' '
   in
    case padMode of
      PadLeft -> padding <> str
      PadRight -> str <> padding
      PadCenter ->
        let halfPad = replicate (div diff 2) ' ' in halfPad <> str <> halfPad
