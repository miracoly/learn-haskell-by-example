module Main (main) where

import qualified Csv
import qualified Data.Maybe as M
import Data.Sliceable (Sliceable (slice))
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
main = do
  mCsv <- parseInFile "in"
  case mCsv of
    Left _ -> putStrLn "no input file given (do so with --in=<path>)"
    Right csv -> do
      mAppend <- eitherToMaybe <$> parseInFile "append"
      mSliceInterval <- Args.getInterval "slice"
      mSearch <- Args.getText "search"
      let mAppendOp = fmap (flip (<>)) mAppend
          mSliceOp = fmap (uncurry slice) mSliceInterval
          mSearchOp = fmap Csv.searchText mSearch
          transformOp =
            foldl
              (\t mOp -> M.fromMaybe id mOp . t)
              id
              [mAppendOp, mSliceOp, mSearchOp]
          dataCsv = transformOp csv
      mOut <- Args.getText "out"
      case mOut of
        Just "-" -> Csv.printCsv dataCsv
        Just fp -> Csv.writeCsv (T.unpack fp) dataCsv
        _ -> do
          _countNonEmpty <- Args.getBool "count-non-empty"
          let mSummary =
                if _countNonEmpty
                  then Just . fmap (T.pack . show) $ Csv.countNonEmpty dataCsv
                  else Nothing
          noPrettyOut <- Args.getBool "no-pretty"
          unless noPrettyOut $
            TIO.putStrLn $
              Csv.prettyText $
                maybe
                  id
                  (flip Csv.unsafeWithSummaries)
                  mSummary
                  (Csv.fromCsv dataCsv)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

when :: Bool -> IO () -> IO ()
when b act = if b then act else return ()

unless :: Bool -> IO () -> IO ()
unless = when . not
