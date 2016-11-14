{-# LANGUAGE DeriveGeneric #-}

module PIParser.PEP.DataTypes where

import Data.Aeson
import GHC.Generics
import PIParser.DataTypes (Sig(..), Param(..))

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
    out :: String,
    groupList :: [InpParam]
} deriving (Generic, Show)

instance FromJSON SigGroup
instance ToJSON SigGroup
instance FromJSON SigGroupList
instance ToJSON SigGroupList
instance FromJSON InpParam
instance FromJSON InpParams
