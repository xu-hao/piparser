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
import Data.Tree
import Data.Vector.Storable (toList)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  parseSourceFile filename ["-Xclang", "-detailed-preprocessing-record"] (\ s -> do
    c <- getCursor s
    mdl <- topMacroDefList c
    sdl <- topStructDeclList c
    tree <- toTree c
    liftIO $ putStrLn (drawTree tree)
    liftIO $ print mdl
    liftIO $ print sdl
    )
  return ()

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
