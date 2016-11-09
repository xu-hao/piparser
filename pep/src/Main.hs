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
import Data.Map (Map, fromList, member, partitionWithKey, keys)
import Text.Regex.TDFA
import Data.Aeson
import GHC.Generics
import StringUtils

data Param = Param {paramType :: String, paramName :: String} deriving (Generic, Show)
data Sig = Sig {funcName :: String, paramList :: [Param]} deriving (Generic, Show)

data SigGroup = SigGroup {groupName :: String, sigList :: [Sig]} deriving (Generic, Show)
data SigGroupList = SigGroupList {sigGroupList :: [SigGroup]} deriving (Generic, Show)

instance FromJSON Param
instance ToJSON Param
instance FromJSON Sig
instance ToJSON Sig
instance FromJSON SigGroup
instance ToJSON SigGroup
instance FromJSON SigGroupList
instance ToJSON SigGroupList

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n l = take n l : chop n (drop n l)

main :: IO ()
main = do
  (out : args) <- getArgs
  let argss = chop 5 args
  sdl <- mapM (\[group, pat, filename, p1, p2] -> do
              print [group, pat, filename, p1, p2]
              sdl <- getLists2 group pat filename p1 p2
              return sdl) argss

  B.writeFile out (encode (SigGroupList sdl))

getLists2 :: String -> String -> String -> String -> String -> IO SigGroup
getLists2 group pat filename p1 p2 = do
  files <- liftIO $ find always (fileName ~~? pat) filename
  lists <- mapM (getLists p1 p2) files
  let sdl = fold lists
  return (SigGroup group sdl)

toSig :: (ClangBase m, MonadIO m) => String -> String -> Cursor s' -> ClangT s m Sig
toSig p1 p2 c = do
  name <- getSpelling c >>= unpack
  n <- getNumArguments c
  args <- mapM (getArgument c >=> getSpelling >=> unpack ) [0..n-1]
  argtypes <- mapM (getArgument c >=> getType >=> getTypeSpelling >=> unpack ) [0..n-1]
  return (Sig (p1 ++ toSnake (drop (length p2) name)) (zipWith Param argtypes args))

getLists :: String -> String -> String -> IO [Sig]
getLists p1 p2 filename = do
    putStrLn ("parsing " ++ filename)
    lists <- parseSourceFile filename ["-Xclang", "-detailed-preprocessing-record", "-DRODS_SERVER", "-I/usr/include/irods"] (\ s -> do
      c <- getCursor s
      -- tree <- toTree filename c
      -- liftIO $ putStrLn (drawTree tree)
      sdl <- topFuncDeclList c
      sdl5 <- filterM isDefinition sdl
      sdl1 <- filterM (\c -> do
                n <- getSpelling c >>= unpack
                return (n =~ ("^" ++ p2 ++ ".*"))) sdl5
      sdl2 <- mapM (toSig p1 p2) sdl1
      return sdl2
      )
    return (fromMaybe (error filename) lists)


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

topFuncDeclList :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m [Cursor s]
topFuncDeclList c = do
  cl <- getChildren c
  let clist = toList cl
  let flat clist =
          concat <$> mapM (\ c -> case getKind c of
              FunctionDeclCursor -> return [c]
              -- PreprocessingDirectiveCursor -> do
              --     children <- getChildren c
              --     let childList = toList children
              --     flat childList
              _ -> return []) clist
  flat clist
