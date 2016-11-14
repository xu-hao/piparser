module StringUtils where

import Data.Text
import Data.Map

showMap :: Map String String -> IO ()
showMap m = do
    mapM_ (\(a,b) -> do
              putStrLn a
              putStrLn b) (toList m)
