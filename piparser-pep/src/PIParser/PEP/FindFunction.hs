{-# LANGUAGE FlexibleContexts #-}

module PIParser.PEP.FindFunction (getSigGroup) where

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
import PIParser.FunctionList
import PIParser.DataTypes (Sig(..), Param(..), InpParams(..))
import PIParser.PEP.DataTypes (SigGroup(..))
import PIParser.Function


getSigGroup :: InpParams -> String -> String -> String -> Map String String -> Map String String -> IO SigGroup
getSigGroup ps group pat filename opmap constmap = do
  files <- liftIO $ find always (fileName ~~? pat) filename
  sdl <- mapM (getLists ps opmap constmap) files
  return (SigGroup group (fold sdl))

getLists :: InpParams -> Map String String -> Map String String -> String -> IO [Sig]
getLists ps opmap constmap filename = do
    let opfunctions = toList opmap
    putStrLn ("parsing " ++ filename)
    lists <- parseSourceFile filename (["-Xclang", "-DRODS_SERVER"] ++ map ("-I" ++) (headers ps)) (\ s -> do
      c <- getCursor s
      sdl <- topFuncDeclList c
      sdl1 <- filterM isDefinition sdl
      fold <$> mapM (\c -> do
                n <- getSpelling c >>= unpack
                let list = filter (\(_, function) -> n == function) opfunctions
                mapM (\(op, _) -> toSig (constmap ! op) c) list) sdl1
      )
    return (fromMaybe (error filename) lists)
