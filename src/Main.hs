{-# LANGUAGE FlexibleContexts #-}

module Main where

import Clang
import Clang.TranslationUnit (getCursor)
import Clang.Cursor
import Clang.String
import Clang.Type (getTypeSpelling)
import Control.Applicative
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
    tree <- toTree c
    liftIO $ putStrLn (drawTree tree)
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
