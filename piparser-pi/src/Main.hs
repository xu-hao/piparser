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
import Data.Map (Map, fromList, member, partitionWithKey, keys)
import PIParser.FunctionList

main :: IO ()
main = do
  args <- getArgs
  let [pat, filename] = args
  (mdl, sdl, tdl) <- getLists2 pat filename
  let (mdl1, mdl2) = partitionWithKey (\k a -> k `elem` map (++ "_PI") sdl) mdl
  let (mdl3, mdl4) = partitionWithKey (\k a -> k `elem` map (++ "_PI") tdl) mdl2
  putStrLn "macro with struct decl"
  print (keys mdl1)
  putStrLn "macro with typedef"
  print (keys mdl3)
  putStrLn "other macro"
  print (keys mdl4)

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
