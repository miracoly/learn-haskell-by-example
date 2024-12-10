module Util.Arguments (module Util.Arguments) where

import qualified Data.List as L
import qualified Data.Text as T
import System.Environment (getArgs)
import qualified Data.Maybe as M

getArguments :: IO [T.Text]
getArguments = fmap (map T.pack) getArgs

getValueOf :: T.Text -> IO (Maybe T.Text)
getValueOf key = undefined -- TODO
  where
    argKey = "--" <> key <> "="

getBool :: T.Text -> IO Bool
getBool key = let argKey = "--" <> key in L.elem argKey <$> getArguments

getChar :: T.Text -> IO (Maybe Char)
getChar = undefined

getText :: T.Text -> IO (Maybe T.Text)
getText = undefined

getInterval :: T.Text -> IO (Maybe (Int, Int))
getInterval = undefined
