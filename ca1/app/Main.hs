
-- By Mateusz Kolodziejczyk 20084190
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
    -- Get the text file and dict file name
    Prelude.putStrLn "Please Enter the name of the textfile"
    textFile <- Prelude.getLine 
    Prelude.putStrLn "Please Enter the name of the dictionary file"
    dictFile <- Prelude.getLine

    textVar <- TIO.readFile textFile
    dictVar <- TIO.readFile dictFile
    -- Zip to store the indexes for later
    let words = Prelude.zip [0..] $ extractWords textVar
    -- Store the dictionary as a list of words
    let dict = extractWords dictVar

    let dictChecker = containsCheck dict

    -- Print out the initial words
    Prelude.putStrLn "Initial Words"
    print $ snd $ unzip words
    -- Split the words into ones in the dictionary/ not in dictionary
    let notInDict = dictChecker Prelude.notElem words
    let inDict = dictChecker Prelude.elem words
    

    -- Print out the words that will be changed

    Prelude.putStrLn "Words not in Dictionary" 
    print $ snd $ unzip notInDict
    -- Fix the words getting user input
    newWords <- fixWords dict notInDict

    -- Print out the changed words only
    Prelude.putStrLn "Changed Words:" 
    print $ snd $ unzip newWords

    -- Concat dict and new words, sort new list by index in the tuple, unzip and get the words only
    let fixedWords =  snd $ unzip $ sortBy (comparing fst) (newWords ++ inDict)


    -- Use the report to print out the list of words in order
    Prelude.putStrLn "Final Report: "
    print fixedWords



