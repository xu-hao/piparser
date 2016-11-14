{-# LANGUAGE FlexibleContexts #-}

module FindFunction (getSigGroup) where

import Clang
import Clang.TranslationUnit (getCursor)
import Clang.Cursor
import Clang.String
import Clang.Type (getTypeSpelling)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.FilePath.Find (find, always, fileName, (~~?))
import Data.Foldable (fold)
import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, member, partitionWithKey, keys, (!), mapWithKey, toList)
import Text.Regex.TDFA
import FunctionList
import DataTypes


getSigGroup :: String -> String -> String -> Map String String -> Map String String -> IO SigGroup
getSigGroup group pat filename opmap constmap = do
  files <- liftIO $ find always (fileName ~~? pat) filename
  sdl <- mapM (getLists opmap constmap) files
  return (SigGroup group (fold sdl))

toSig :: (ClangBase m, MonadIO m) => String -> Cursor s' -> ClangT s m Sig
toSig op c = do
  name <- getSpelling c >>= unpack
  n <- getNumArguments c
  args <- mapM (getArgument c >=> getSpelling >=> unpack ) [0..n-1]
  argtypes <- mapM (getArgument c >=> getType >=> getTypeSpelling >=> unpack ) [0..n-1]
  return (Sig op (zipWith Param argtypes args))

getLists :: Map String String -> Map String String -> String -> IO [Sig]
getLists opmap constmap filename = do
    let opfunctions = toList opmap
    putStrLn ("parsing " ++ filename)
    lists <- parseSourceFile filename ["-Xclang", "-DRODS_SERVER", "-I/usr/include/irods"] (\ s -> do
      c <- getCursor s
      sdl <- topFuncDeclList c
      sdl1 <- filterM isDefinition sdl
      fold <$> mapM (\c -> do
                n <- getSpelling c >>= unpack
                let list = filter (\(_, function) -> n == function) opfunctions
                mapM (\(op, _) -> toSig (constmap ! op) c) list) sdl1
      )
    return (fromMaybe (error filename) lists)
