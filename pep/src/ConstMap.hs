{-# LANGUAGE FlexibleContexts #-}

module ConstMap where

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
import DataTypes
import Tree
import ClangUtils
import Data.Char


toMap :: (ClangBase m, MonadIO m) => String -> Cursor s' -> ClangT s m (String, String)
toMap filename c = do
    sp1 <- getSpelling c
    sp <- unpack sp1
    des <- getDescendants c
    let desl = toList des
    let desl1 = filter (\c -> case getKind c of
                            StringLiteralCursor -> True
                            _ -> False) desl
    let strc = head desl1
    strlit <- getStringLiteral filename strc
    return (sp, strlit)

getConstMap :: String -> IO (Map String String)
getConstMap filename = do
    putStrLn ("parsing " ++ filename ++ " for constant map")
    lists <- parseSourceFile filename ["-Xclang", "-detailed-preprocessing-record", "-DRODS_SERVER", "-I/usr/include/irods"] (\ s -> do
      cl <- getDeclarations s
      let sdl = toList cl
      let sdl1 = filter (\c -> case getKind c of
                                VarDeclCursor -> True
                                _ -> False) sdl
      sdl2 <- filterM (\c -> do
                    sp <- getSpelling c >>= unpack
                    return (isUpper (head sp))) sdl1
      sdl3 <- mapM (toMap filename) sdl2
      return (fromList sdl3)
      )
    return (fromMaybe (error filename) lists)
