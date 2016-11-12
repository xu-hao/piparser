{-# LANGUAGE FlexibleContexts #-}

module ClangUtils where

import Clang
import Clang.TranslationUnit (getCursor, getDiagnosticSet)
import Clang.Cursor
import Clang.File (getName)
import Clang.Location (getFileLocation)
import Clang.Range
import Clang.String
import Clang.Type (getTypeSpelling)
import qualified Clang.Diagnostic as CD
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Tree
import Data.Vector.Storable (toList)
import Data.List (intercalate)
import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, member, partitionWithKey, keys)

getStringLiteral :: (ClangBase m, MonadIO m) => String -> Cursor s' -> ClangT s m String
getStringLiteral filename strc = do
  range <- getExtent strc
  start <- getStart range
  end <- getEnd range
  (_, sline, scol, spos) <- getFileLocation start
  (_, eline, ecol, epos) <- getFileLocation end
  cont <- liftIO $ readFile filename
  return (take (epos - spos - 2) (drop (spos + 1) cont))

printDiagnostics :: (ClangBase m, MonadIO m) => TranslationUnit s' -> ClangT s m ()
printDiagnostics s = do
  ds <- getDiagnosticSet s
  dsel <- CD.getElements ds
  mapM_ (\d -> do
            sp <- CD.getSpelling d >>= unpack
            liftIO $ putStrLn sp
            ) dsel

toTree :: (ClangBase m, MonadIO m) => FilePath -> Cursor s' -> ClangT s m (Tree String)
toTree fp c = do
  cl <- getChildren c
  let clist = toList cl
      s = getKind c
  p <- getSpelling c >>= unpack
  t <- case s of
      FunctionDeclCursor -> do
          n <- getNumArguments c
          args <- mapM (getArgument c >=> getSpelling >=> unpack ) [0..n-1]
          argtypes <- mapM (getArgument c >=> getType >=> getTypeSpelling >=> unpack ) [0..n-1]
          return (show (zip args argtypes))
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

printTree :: FilePath -> IO String
printTree filename = do
  t <- parseSourceFile filename ["-Xclang", "-DRODS_SERVER", "-DCREATE_API_TABLE_FOR_SERVER", "-I/usr/include/irods", "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include/", "-I/opt/irods-externals/boost1.60.0-0/include", "-I/opt/irods-externals/jansson2.7-0/include", "-std=c++11"] (\ s -> do
    printDiagnostics s
    c <- getCursor s
    chl <- toList <$> getChildren c
    let chl2 = filter (\c -> case getKind c of
                      VarDeclCursor -> True
                      _ -> False) chl
    chl3 <- mapM (toTree filename >=> return . drawTree) chl2
    return (intercalate "\n" chl3)
    )
  return (fromMaybe (error filename) t)
