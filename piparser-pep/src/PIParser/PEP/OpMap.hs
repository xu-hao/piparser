{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module PIParser.PEP.OpMap (getOpMap) where

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
import PIParser.ClangUtils
import PIParser.Templates
import PIParser.DataTypes


toMap :: (ClangBase m, MonadIO m) => Cursor s' -> ClangT s m (String, String)
toMap c = do
        desl2 <- toList <$> getChildren c
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

getOpMap :: InpParams -> String -> IO (Map String String)
getOpMap ps filename = do
    putStrLn ("parsing " ++ filename ++ " for operation map")
    lists <- parseSourceFile filename (["-Xclang", "-DRODS_SERVER", "-std=c++11"] ++ map ("-I" ++) (headers ps)) (\ s -> do
      printDiagnostics s
      sdl <- toList <$> getDeclarations s
      let sdl1 = $(filterByKind [p|FunctionDeclCursor|] [|sdl|])
      [sdl2] <- $(filterBySpelling [p|"plugin_factory"|] [|sdl1|])
      sdl3 <- toList <$> (getDefinition sdl2 >>= getChildren)
      let [sdl4] = $(filterByKind [p|CompoundStmtCursor|] [|sdl3|])
      sdl5 <- toList <$> getChildren sdl4
      let sdl6 = $(filterByKind [p|UnexposedExprCursor|] [|sdl5|])
      sdl7 <- mapM (\c -> do
        [desl] <- toList <$> getChildren c
        [desl1] <- toList <$> getChildren desl
        return desl1
        ) sdl6
      sdl8 <- $(filterBySpelling [p|"add_operation"|] [|sdl7|])
      sdl9 <- mapM toMap sdl8
      return (fromList sdl9)
      )
    return (fromMaybe (error filename) lists)
