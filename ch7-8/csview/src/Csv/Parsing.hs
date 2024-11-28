module Csv.Parsing (module Csv.Parsing) where

import Csv.Types
import qualified Data.List as L
import qualified Data.Text as T
import Text.Read (readMaybe)

data Separators = Separators
  { sepLineSeparator :: Char,
    sepFieldSeparator :: Char
  }
  deriving (Eq, Show)

data HeaderOption = WithHeader | WithoutHeader
  deriving (Eq, Show)

data CsvParseOptions = CsvParseOptions
  { cpoSeparators :: Separators,
    cpoHeaderOptions :: HeaderOption
  }
  deriving (Eq, Show)

defaultSeparators :: Separators
defaultSeparators =
  Separators
    { sepLineSeparator = '\n',
      sepFieldSeparator = ','
    }

defaultOptions :: CsvParseOptions
defaultOptions =
  CsvParseOptions
    { cpoSeparators = defaultSeparators,
      cpoHeaderOptions = WithoutHeader
    }

parseCsv :: CsvParseOptions -> T.Text -> Either String Csv
parseCsv CsvParseOptions {..} raw =
  case lines' of
    [] -> mkCsv Nothing []
    ((_, firstLine) : rest) ->
      let expectedLength = length $ splitFields' firstLine
       in case cpoHeaderOptions of
            WithHeader ->
              let headerFields = Just (splitFields' firstLine)
               in unsafeMkCsv headerFields <$> parseColumns cpoSeparators expectedLength rest
            WithoutHeader ->
              unsafeMkCsv Nothing <$> parseColumns cpoSeparators expectedLength lines'
  where
    lines' = splitLines cpoSeparators raw
    splitFields' = splitFields cpoSeparators

splitFields :: Separators -> T.Text -> [T.Text]
splitFields sep =
  let separator = T.singleton $ sepFieldSeparator sep
   in L.map T.strip . T.splitOn separator

parseColumns :: Separators -> Int -> [(Int, T.Text)] -> Either String [[DataField]]
parseColumns separators expectedLength lines' =
  let textColumns = L.transpose <$> L.foldl' parseRow (Right []) lines'
   in fmap (L.map (L.map textToDataField)) textColumns
  where
    parseRow ::
      Either String [[T.Text]] ->
      (Int, T.Text) ->
      Either String [[T.Text]]
    parseRow mRows (lNum, line) =
      mRows
        >>= ( \rows ->
                let fields = splitFields separators line
                 in if length fields /= expectedLength
                      then Left $ errorMsg lNum fields
                      else Right $ rows ++ [fields]
            )
    errorMsg :: Int -> [T.Text] -> String
    errorMsg lNum fields =
      "Number of fields in line "
        <> show lNum
        <> " does not match"
        <> " expected length of "
        <> show expectedLength
        <> "! Actual length is "
        <> show (length fields)
        <> "!"

splitLines :: Separators -> T.Text -> [(Int, T.Text)]
splitLines Separators {sepLineSeparator} raw =
  L.filter (\(_, t) -> not $ T.null t) $
    L.zip [1 ..] $
      T.split (== sepLineSeparator) raw

textToDataField :: T.Text -> DataField
textToDataField txt =
  case txt of
    "" -> NullValue
    _ ->
      let mInt = readMaybe (T.unpack txt)
       in maybe (TextValue txt) IntValue mInt

dataFieldToText :: DataField -> T.Text
dataFieldToText df =
  case df of
    IntValue i -> T.pack $ show i
    TextValue txt -> txt
    NullValue -> ""
