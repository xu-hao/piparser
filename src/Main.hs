{-# LANGUAGE FlexibleContexts #-}

module Main where

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
import Data.List (nub, partition)
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map, fromList, member, partitionWithKey)

main :: IO ()
main = do
  args <- getArgs
  let [pat, filename] = args
  (mdl, sdl, tdl) <- getLists2 pat filename
  let (mdl1, mdl2) = partitionWithKey (\k a -> k `elem` map (++ "_PI") sdl) mdl
  let (mdl3, mdl4) = partitionWithKey (\k a -> k `elem` map (++ "_PI") tdl) mdl2
  putStrLn "macro with struct decl"
  print mdl1
  putStrLn "macro with typedef"
  print mdl3
  putStrLn "other macro"
  print mdl4

getLists2 :: String -> String -> IO (Map String String, [String], [String])
getLists2 pat filename = do
  files <- liftIO $ find always (fileName ~~? pat) filename
  lists <- mapM getLists files
  let (mdl, sdl, tdl) = fold lists
  let sdl2 = filter (\s -> (s ++ "_PI") `member` mdl) sdl
  let tdl2 = filter (\s -> (s ++ "_PI") `member` mdl) tdl
  return (mdl, nub sdl2, nub tdl2)

getLists :: String -> IO (Map String String, [String], [String])
getLists filename = do
    putStrLn ("parsing " ++ filename)
    lists <- parseSourceFile filename ["-Xclang", "-detailed-preprocessing-record"] (\ s -> do
      c <- getCursor s
      -- tree <- toTree filename c
      -- liftIO $ putStrLn (drawTree tree)
      mdl <- topMacroDefList filename c
      sdl <- topStructDeclList c
      tdl <- topTypedefDeclList c
      return (mdl, sdl, tdl)
      )
    return (fromMaybe (error filename) lists)


toTree :: (ClangBase m, MonadIO m) => FilePath -> Cursor s' -> ClangT s m (Tree String)
toTree fp c = do
  cl <- getChildren c
  let clist = toList cl
      s = getKind c
  p <- getSpelling c >>= unpack
  t <- case s of
      FieldDeclCursor -> getType c >>= getTypeSpelling >>= unpack
      StringLiteralCursor -> return "string"
      MacroDefinitionCursor -> do
        extent <- getExtent c
        st <- getStart extent
        en <- getEnd extent
        (file1, a1, b1, c1) <- getFileLocation st
        (file2, a2, b2, c2) <- getFileLocation en
        bs <- liftIO $ withFile fp ReadMode (\h -> do
          hSeek h AbsoluteSeek (fromIntegral c1)
          BS.hGet h (c2 - c1))
        return (BS.unpack bs)
      _ -> return ""
  Node (show s ++ " " ++ show p ++ " " ++ t) <$> mapM (toTree fp) clist

topMacroDefList :: (ClangBase m, MonadIO m) => FilePath -> Cursor s' -> ClangT s m (Map String String)
topMacroDefList fp c = do
  cl <- getChildren c
  let clist = toList cl
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
    ) (filter (\ c -> case getKind c of
              MacroDefinitionCursor -> True
              _ -> False) clist)
  return (fromList (filter (\(s, _) -> drop (length s - 3) s == "_PI") mdl))

topStructDeclList :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m [String]
topStructDeclList c = do
  cl <- getChildren c
  let clist = toList cl
  sdl <- mapM (getSpelling >=> unpack) (filter (\ c -> case getKind c of
              StructDeclCursor -> True
              _ -> False) clist)
  return sdl

topTypedefDeclList :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m [String]
topTypedefDeclList c = do
  cl <- getChildren c
  let clist = toList cl
  sdl <- mapM (getSpelling >=> unpack) (filter (\ c -> case getKind c of
              TypedefDeclCursor -> True
              _ -> False) clist)
  return (map (\s -> take (length s - 2) s) sdl)
