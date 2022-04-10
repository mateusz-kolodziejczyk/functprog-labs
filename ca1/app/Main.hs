module Main where

import TextRead
import Data.Text.IO as TIO
import Data.Text as T
import Control.Monad
import Data.List (sortBy)
import Data.Function (on)
import Data.Ord (comparing)
main :: IO ()
main = do
    textVar <- TIO.readFile "example-text.txt"
    dictVar <- TIO.readFile "english.txt"
    -- Zip to store the indexes for later
    let words = Prelude.zip [0..] $ extractWords textVar

    let dict = extractWords dictVar
    let dictChecker = containsCheck dict

    let notInDict = dictChecker Prelude.notElem words
    let inDict = dictChecker Prelude.elem words
    
    newWords <- fixWords dict notInDict
    let fixedWords = sortBy (comparing fst) (newWords ++ inDict)
    print fixedWords

-- Taken from https://stackoverflow.com/questions/30380697/sort-tuples-by-one-of-their-elements-in-haskell
indexSort :: Ord a => [(a, b)] -> [(a, b)]
indexSort = sortBy (compare `on` fst)

fixWords :: [Text] -> [(Int, Text)] -> IO [(Int, Text)]
fixWords dict words = do
    let unzipped = unzip words
    let indexes = fst unzipped
    let w = snd unzipped
    newW <- mapM (\x -> getWord dict) w
    let y = Prelude.zip indexes newW
    return y

getWord :: [Text] -> IO Text
getWord dict = do
    w <- TIO.getLine
    if Prelude.elem w dict 
        then do
            print "inputed"
            return w
        else do
            print "Not Found in Dictionary"
            getWord dict


