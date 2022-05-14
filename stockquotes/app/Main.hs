{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)
import qualified Data.Vector as V

import QuoteData
import StatReport
import HtmlReport

main ::  IO ()
main  = do
  print "hello"
  -- putStrLn "Please enter the filepath/name of the csv file:"
  -- fname <- getLine 
  -- putStrLn "Please enter the name of the html target file"
  -- htmlFile <- getLine
  -- csvData <- BL.readFile fname
  -- case decodeByName csvData of
  --   Left err -> putStrLn err
  --   Right (_, quotes) -> generateReports htmlFile quotes


generateReports :: (Functor t, Foldable t) =>
                 String ->  t QuoteData -> IO ()
generateReports htmlFl quotes = do
  putStr textRpt
  BL.writeFile htmlFl htmlRpt
 where
   statInfo' = statInfo quotes
   textRpt = textReport statInfo'
   htmlRpt = htmlReport  quotes statInfo' 




-- you can use this 
readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (V.toList quotes)
    
