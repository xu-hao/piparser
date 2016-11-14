module Main where

import Clang.String (unpack)
import Clang.Cursor (getSpelling)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, member, partitionWithKey, keys, (!), mapWithKey)
import Data.Aeson
import PIParser.ClangUtils
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  [arg, arg2, arg3] <- getArgs
  ps <- fromJust . decode <$> B.readFile arg
  t <- showTree ps arg2 (case arg3 of
    "" -> \ _ _ -> return True
    _ -> \ _ c -> do
                      sp <- getSpelling c >>= unpack
                      return (sp =~ arg3))
  putStrLn t
