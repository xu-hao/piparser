{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import Data.Aeson
import GHC.Generics

data Param = Param {paramType :: String, paramName :: String} deriving (Generic, Show)
data Sig = Sig {funcName :: String, paramList :: [Param]} deriving (Generic, Show)

data SigGroup = SigGroup {groupName :: String, sigList :: [Sig]} deriving (Generic, Show)
data SigGroupList = SigGroupList {sigGroupList :: [SigGroup]} deriving (Generic, Show)

data InpParam = InpParam {
    name :: String,
    ty :: String,
    pattern :: String,
    directory :: String,
    opFile :: String,
    constFile :: String
} deriving (Generic, Show)

data InpParams = InpParams {
    headers :: [String],
    out :: String,
    groupList :: [InpParam]
} deriving (Generic, Show)

instance FromJSON Param
instance ToJSON Param
instance FromJSON Sig
instance ToJSON Sig
instance FromJSON SigGroup
instance ToJSON SigGroup
instance FromJSON SigGroupList
instance ToJSON SigGroupList
instance FromJSON InpParam
instance FromJSON InpParams
