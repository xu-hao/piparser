{-# LANGUAGE TemplateHaskell #-}

module PIParser.Templates where

import Clang
import Clang.TranslationUnit (getCursor, getDiagnosticSet)
import Clang.Cursor
import Clang.File (getName)
import Clang.Location (getFileLocation)
import Clang.Range
import Clang.String
import Clang.Type (getTypeSpelling)
import Language.Haskell.TH
import Control.Monad

filterByKind :: Q Pat -> Q Exp -> Q Exp
filterByKind pat exp = [| filter (\c -> case getKind c of
                          $(pat) -> True
                          _ -> False) $(exp) |]


filterBySpelling :: Q Pat -> Q Exp -> Q Exp
filterBySpelling pat exp = [| filterM (\c -> do
              sp <- getSpelling c >>= unpack
              return (case sp of
                          $(pat) -> True
                          _ -> False)) $(exp) |]
