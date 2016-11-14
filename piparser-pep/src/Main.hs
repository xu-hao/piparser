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
import PIParser.PEP.DataTypes
import PIParser.ConstMap
import PIParser.PEP.OpMap
import PIParser.PEP.OpMap2
import PIParser.PEP.FindFunction

main :: IO ()
main = do
  [arg, arg2] <- getArgs
  ps <- fromJust . decode <$> B.readFile arg
  ps2 <- fromJust . decode <$> B.readFile arg2
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
                      getSigGroup ps group pat filename opmap constmap) (groupList ps2)

  B.writeFile (out ps2) (encode (SigGroupList sdl))
