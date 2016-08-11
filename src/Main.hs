{-# LANGUAGE FlexibleContexts #-}

module Main where

import Clang
import Clang.TranslationUnit (getCursor)
import Clang.Cursor
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

main :: IO ()
main = do
  args <- getArgs
  let [pat, filename] = args
  lists <- getLists2 pat filename
  print lists

getLists2 :: String -> String -> IO ([String], [String])
getLists2 pat filename = do
  files <- liftIO $ find always (fileName ~~? pat) filename
  lists <- mapM getLists files
  return (fold lists)

getLists :: String -> IO ([String], [String])
getLists filename = do
    putStrLn ("parsing " ++ filename)
    lists <- parseSourceFile filename ["-Xclang", "-detailed-preprocessing-record"] (\ s -> do
      c <- getCursor s
      -- tree <- toTree c
      -- liftIO $ putStrLn (drawTree tree)
      mdl <- topMacroDefList c
      sdl <- topStructDeclList c
      return (mdl, sdl)
      )
    return (fromMaybe (error filename) lists)


toTree :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m (Tree String)
toTree c = do
  cl <- getChildren c
  let clist = toList cl
      s = getKind c
  p <- getSpelling c >>= unpack
  t <- case s of
      FieldDeclCursor -> getType c >>= getTypeSpelling >>= unpack
      StringLiteralCursor -> return "string"
      _ -> return ""
  Node (show s ++ " " ++ show p ++ " " ++ t) <$> mapM toTree clist

topMacroDefList :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m [String]
topMacroDefList c = do
  cl <- getChildren c
  let clist = toList cl
  mdl <- mapM (getSpelling >=> unpack) (filter (\ c -> case getKind c of
              MacroDefinitionCursor -> True
              _ -> False) clist)
  return (filter (\s -> drop (length s - 3) s == "_PI") mdl)

topStructDeclList :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m [String]
topStructDeclList c = do
  cl <- getChildren c
  let clist = toList cl
  sdl <- mapM (getSpelling >=> unpack) (filter (\ c -> case getKind c of
              StructDeclCursor -> True
              _ -> False) clist)
  return (filter (\s -> drop (length s - 2) s == "_t") sdl)
