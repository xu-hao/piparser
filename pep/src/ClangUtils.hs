{-# LANGUAGE FlexibleContexts #-}

module ClangUtils where

import Clang
import Clang.TranslationUnit (getCursor)
import Clang.Cursor
import Clang.File (getName)
import Clang.Location (getFileLocation)
import Clang.Range
import Clang.String
import Clang.Type (getTypeSpelling)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Tree
import Data.Vector.Storable (toList)
import System.Environment
import System.FilePath.Find (find, always, fileName, (~~?))
import Data.Foldable (fold)
import Data.List (nub, partition, intercalate)
import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, member, partitionWithKey, keys)
import Data.Char

getStringLiteral :: (ClangBase m, MonadIO m) => String -> Cursor s' -> ClangT s m String
getStringLiteral filename strc = do
  range <- getExtent strc
  start <- getStart range
  end <- getEnd range
  (_, sline, scol, spos) <- getFileLocation start
  (_, eline, ecol, epos) <- getFileLocation end
  cont <- liftIO $ readFile filename
  return (take (epos - spos - 2) (drop (spos + 1) cont))
