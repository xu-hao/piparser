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

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n l = take n l : chop n (drop n l)

main :: IO ()
main = do
  (out : args) <- getArgs
  let argss = chop 6 args
  sdl <- mapM (\[group, ty, pat, filename, opfile, constfile] -> do
              print [group, ty, pat, filename, opfile, constfile]
              case ty of
                  "api" -> do
                      opmap <- getOpMap2 opfile
                      let constmap = mapWithKey (\k _ -> k) opmap
                      getSigGroup group pat filename opmap constmap
                  _ -> do
                      opmap <- getOpMap opfile
                      constmap <- getConstMap constfile
                      getSigGroup group pat filename opmap constmap) argss

  B.writeFile out (encode (SigGroupList sdl))
