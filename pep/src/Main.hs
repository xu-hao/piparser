module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, member, partitionWithKey, keys, (!), mapWithKey)
import Data.Aeson
import DataTypes
import ConstMap
import OpMap
import OpMap2
import FindFunction

main :: IO ()
main = do
  [args] <- getArgs
  ps <- fromJust . decode <$> B.readFile args
  sdl <- mapM (\p@(InpParam group ty pat filename opfile constfile) -> do
              print p
              case ty of
                  "api" -> do
                      opmap <- getOpMap2 ps opfile
                      let constmap = mapWithKey (\k _ -> k) opmap
                      getSigGroup ps group pat filename opmap constmap
                  _ -> do
                      opmap <- getOpMap ps opfile
                      constmap <- getConstMap ps constfile
                      getSigGroup ps group pat filename opmap constmap) (groupList ps)

  B.writeFile (out ps) (encode (SigGroupList sdl))
