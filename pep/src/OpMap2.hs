{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module OpMap2 (getOpMap2) where

import Clang
import Clang.TranslationUnit (getCursor, getDiagnosticSet)
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
import DataTypes

toMap :: (ClangBase m, MonadIO m) => String -> Cursor s' -> ClangT s m (String, String)
toMap filename c = do
    -- t <- drawTree <$> toTree filename c
    -- liftIO $ putStrLn t
    [_,_,_,_,_,_,_,_,func, oper,_,_] <- toList <$> getChildren c

    [oper1] <- toList <$> getChildren oper
    op <- getStringLiteral filename oper1

    [_, _, func1] <- toList <$> getChildren func
    [func2] <- toList <$> getChildren func1
    [func3, _, _] <- toList <$> getChildren func2
    [func4] <- toList <$> getChildren func3
    l <- toList <$> getChildren func4
    let func5 = last l
    [func6] <- toList <$> getChildren func5
    [func7] <- toList <$> getChildren func6
    [func8] <- toList <$> getChildren func7
    function <- getSpelling func8 >>= unpack
    return (op, function)

getOpMap2 :: InpParams -> String -> IO (Map String String)
getOpMap2 ps filename = do
    putStrLn ("parsing " ++ filename ++ " for operation map")
    lists <- parseSourceFile filename (["-Xclang", "-DRODS_SERVER", "-DCREATE_API_TABLE_FOR_SERVER", "-std=c++11"] ++ map ("-I" ++) (headers ps)) (\ s -> do
      printDiagnostics s
      c <- getCursor s
      chl <- toList <$> getChildren c
      let chl2 = $(filterByKind [p|VarDeclCursor|] [|chl|])
      [sdl2] <- $(filterBySpelling [p|"server_api_table_inp"|] [|chl2|])
      [_, _, sdl3] <- toList <$> (getChildren sdl2)
      [sdl4] <- toList <$> (getChildren sdl3)
      sdl5 <- toList <$> (getChildren sdl4)
      sdl6 <- mapM (toMap filename) sdl5
      return (fromList sdl6)
      )
    return (fromMaybe (error filename) lists)
