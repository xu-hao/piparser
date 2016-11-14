{-# LANGUAGE DeriveGeneric #-}

module PIParser.DataTypes where

import Data.Aeson
import GHC.Generics

data Param = Param {paramType :: String, paramName :: String} deriving (Generic, Show)
data Sig = Sig {funcName :: String, paramList :: [Param]} deriving (Generic, Show)

data InpParams = InpParams {
    headers :: [String]
} deriving (Generic, Show)

instance FromJSON InpParams
instance ToJSON InpParams
instance FromJSON Param
instance ToJSON Param
instance FromJSON Sig
instance ToJSON Sig
