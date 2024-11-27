module Csv.Parsing (module Csv.Parsing) where

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
