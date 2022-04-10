module Main where

import TextRead
import Data.Text.IO as TIO
import Data.Text as T

main :: IO ()
main = do
    -- textVar <- TIO.readFile "example-text.txt"
    -- print $ allWordsReport "Cool" $ extractWords textVar
    -- print $ T.unpack $ allWordsReport "Cool" $ extractWords $ T.pack "Very cool set of words"
    -- print $ extractWords textVar
    print $ stringr $ T.pack "Toodle"
