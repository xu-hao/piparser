{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module FunctionList where

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
import Data.Vector.Storable (toList)
import System.Environment
import System.FilePath.Find (find, always, fileName, (~~?))
import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, member, partitionWithKey, keys, (!), mapWithKey)
import Templates

topMacroDefList :: (ClangBase m, MonadIO m) => FilePath -> Cursor s' -> ClangT s m (Map String String)
topMacroDefList fp c = do
  clist <- toList <$> getChildren c
  mdl <- mapM (\c -> do
    key <- getSpelling c >>= unpack
    extent <- getExtent c
    st <- getStart extent
    en <- getEnd extent
    (file1, a1, b1, c1) <- getFileLocation st
    (file2, a2, b2, c2) <- getFileLocation en
    bs <- liftIO $ withFile fp ReadMode (\h -> do
      hSeek h AbsoluteSeek (fromIntegral c1)
      BS.hGet h (c2 - c1))
    let val = BS.unpack bs
    return (key, drop (length key + 1) val)
    ) $(filterByKind [p|MacroDefinitionCursor|] [|clist|])
  return (fromList (filter (\(s, _) -> drop (length s - 3) s == "_PI") mdl))

topStructDeclList :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m [String]
topStructDeclList c = do
  clist <- toList <$> getChildren c
  sdl <- mapM (getSpelling >=> unpack) $(filterByKind [p|StructDeclCursor|] [|clist|])
  return sdl

topTypedefDeclList :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m [String]
topTypedefDeclList c = do
  clist <- toList <$> getChildren c
  sdl <- mapM (getSpelling >=> unpack) $(filterByKind [p|TypedefDeclCursor|] [|clist|])
  return (map (\s -> take (length s - 2) s) sdl)


topFuncDeclList :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m [Cursor s]
topFuncDeclList c = do
  clist <- toList <$> getChildren c
  return $(filterByKind [p|FunctionDeclCursor|] [|clist|])
