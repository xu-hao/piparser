{-# LANGUAGE FlexibleContexts #-}

module PIParser.ConstMap (getConstMap) where

import Clang
import Clang.TranslationUnit (getCursor)
import Clang.Cursor
import Clang.String
import Clang.Type (getTypeSpelling)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Vector.Storable (toList)
import Data.List (nub, partition, intercalate)
import System.IO
import Data.Map (Map, fromList, member, partitionWithKey, keys)
import Data.Char
import PIParser.ClangUtils
import PIParser.DataTypes

toMap :: (ClangBase m, MonadIO m) => String -> Cursor s' -> ClangT s m (String, String)
toMap filename c = do
    sp <- getSpelling c >>= unpack
    desl <- toList <$> getDescendants c
    let [strc] = filter (\c -> case getKind c of
                            StringLiteralCursor -> True
                            _ -> False) desl
    strlit <- getStringLiteral filename strc
    return (sp, strlit)

getConstMap :: InpParams -> String -> IO (Map String String)
getConstMap ps filename = do
    putStrLn ("parsing " ++ filename ++ " for constant map")
    lists <- parseSourceFile filename (["-Xclang", "-detailed-preprocessing-record", "-DRODS_SERVER"] ++ map ("-I" ++) (headers ps)) (\ s -> do
      printDiagnostics s
      sdl <- toList <$> getDeclarations s
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
