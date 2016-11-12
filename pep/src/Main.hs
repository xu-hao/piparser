{-# LANGUAGE FlexibleContexts, DeriveGeneric #-}

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
import Data.List (nub, partition, intercalate)
import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, member, partitionWithKey, keys, (!), mapWithKey)
import Text.Regex.TDFA
import Data.Aeson
import GHC.Generics
import DataTypes
import ConstMap
import OpMap
import OpMap2
import FindFunction

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n l = take n l : chop n (drop n l)

main :: IO ()
main = do
  (out : args) <- getArgs
  let argss = chop 6 args
  sdl <- mapM (\[group, ty, pat, filename, opfile, constfile] -> do
              print [group, ty, pat, filename, opfile, constfile]
              case ty of
                  "api" -> do
                      opmap <- getOpMap2 opfile
                      let constmap = mapWithKey (\k _ -> k) opmap
                      getLists2 group pat filename opmap constmap
                  _ -> do
                      opmap <- getOpMap opfile
                      constmap <- getConstMap constfile
                      getLists2 group pat filename opmap constmap) argss

  B.writeFile out (encode (SigGroupList sdl))
