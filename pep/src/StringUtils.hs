module StringUtils where

import Cases
import Data.Text

toSnake :: String -> String
toSnake = unpack . snakify . pack
