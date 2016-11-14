{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module OpMap (getOpMap) where

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
import System.IO
import Data.Map (Map, fromList, member, partitionWithKey, keys)
import ClangUtils
import Templates


toMap :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m (String, String)
toMap c = do
    [desl] <- toList <$> getChildren c
    [desl1] <- toList <$> getChildren desl
    sp <- getSpelling desl1 >>= unpack
    if sp == "add_operation"
      then do
        desl2 <- toList <$> getChildren desl1
        op <- getSpelling (desl2 !! 1) >>= unpack
        [desl3] <- toList <$> getChildren (desl2 !! 2)
        [desl4] <- toList <$> getChildren desl3
        [desl5] <- toList <$> getChildren desl4
        l <- toList <$> getChildren desl5
        let desl6 = last l
        [desl7] <- toList <$> getChildren desl6
        [desl8] <- toList <$> getChildren desl7
        [desl9] <- toList <$> getChildren desl8
        function <- getSpelling desl9 >>= unpack
        return (op, function)
      else do
        return ("", "")

getOpMap :: String -> IO (Map String String)
getOpMap filename = do
    putStrLn ("parsing " ++ filename ++ " for operation map")
    lists <- parseSourceFile filename ["-Xclang", "-DRODS_SERVER", "-I/usr/include/irods", "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include/", "-I/opt/irods-externals/boost1.60.0-0/include", "-std=c++11"] (\ s -> do
      printDiagnostics s
      sdl <- toList <$> getDeclarations s
      let sdl1 = $(filterByKind [p|FunctionDeclCursor|] [|sdl|])
      sdl2 <- $(filterBySpelling [p|"plugin_factory"|] [|sdl1|])
      sdl3 <- toList <$> (getDefinition (head sdl2) >>= getChildren)
      let [sdl4] = $(filterByKind [p|CompoundStmtCursor|] [|sdl3|])
      sdl5 <- toList <$> getChildren sdl4
      let sdl6 = $(filterByKind [p|UnexposedExprCursor|] [|sdl5|])
      sdl7 <- filter (\(a,_) -> a /= "") <$> mapM toMap sdl6
      return (fromList sdl7)
      )
    return (fromMaybe (error filename) lists)
