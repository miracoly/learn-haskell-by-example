module Util.Arguments (module Util.Arguments) where

import qualified Data.List as L
import qualified Data.Text as T
import System.Environment (getArgs)
import Text.Read (readMaybe)

getArguments :: IO [T.Text]
getArguments = fmap (map T.pack) getArgs

getValueOf :: T.Text -> IO (Maybe T.Text)
getValueOf key =
  let argKey = "--" <> key <> "="
   in do
        args <- getArguments
        return $ L.find (T.isPrefixOf argKey) args >>= T.stripPrefix argKey

getBool :: T.Text -> IO Bool
getBool key = let argKey = "--" <> key in L.elem argKey <$> getArguments

getChar :: T.Text -> IO (Maybe Char)
getChar key = do
  mArg <- getValueOf key
  return $ mArg >>= T.uncons >>= fstIfOne
  where
    fstIfOne :: (Char, T.Text) -> Maybe Char
    fstIfOne (c, rest) = if T.null rest then Just c else Nothing

getText :: T.Text -> IO (Maybe T.Text)
getText = getValueOf

getInterval :: T.Text -> IO (Maybe (Int, Int))
getInterval key = do
  mArg <- fmap T.strip <$> getValueOf key
  return $ do
    (mI1, mI2) <- T.breakOn "," <$> mArg
    i1 <- readMaybe $ T.unpack mI1
    i2 <- readMaybe $ T.unpack $ T.drop 1 mI2
    return (i1, i2)
