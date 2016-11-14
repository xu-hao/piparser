{-# LANGUAGE FlexibleContexts #-}

module PIParser.Function (toSig) where

import Clang
import Clang.TranslationUnit (getCursor)
import Clang.Cursor
import Clang.String
import Clang.Type (getTypeSpelling)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Foldable (fold)
import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, member, partitionWithKey, keys, (!), mapWithKey, toList)
import Text.Regex.TDFA
import PIParser.DataTypes (Sig(..), Param(..))


toSig :: (ClangBase m, MonadIO m) => String -> Cursor s' -> ClangT s m Sig
toSig op c = do
  name <- getSpelling c >>= unpack
  n <- getNumArguments c
  args <- mapM (getArgument c >=> getSpelling >=> unpack ) [0..n-1]
  argtypes <- mapM (getArgument c >=> getType >=> getTypeSpelling >=> unpack ) [0..n-1]
  return (Sig op (zipWith Param argtypes args))
