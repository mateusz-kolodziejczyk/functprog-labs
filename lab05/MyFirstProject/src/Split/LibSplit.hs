module Split.LibSplit where

import Data.Char(toUpper)    
import Data.List.Split(splitOn)    -- this needs to go into dependencies.


ourSplit :: String -> [String]
ourSplit xs = splitOn "A" (map toUpper xs)
