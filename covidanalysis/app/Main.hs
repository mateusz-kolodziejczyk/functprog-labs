{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)
import qualified Data.Vector as V

import CovidData
import StatReport
import HtmlReport

main ::  IO ()
main  = do
  putStrLn "Please enter the filepath/name of the csv file:"
  fname <- getLine 
  putStrLn "Please enter the name of the html target file"
  htmlFile <- getLine
  covidData <- readData fname
  generateReports htmlFile covidData


generateReports :: (Functor t, Foldable t) =>
                 String ->  t CovidData -> IO ()
generateReports htmlFl covidData = do
  putStr textRpt
  --BL.writeFile htmlFl htmlRpt
 where
   statInfo' = statInfo covidData
   textRpt = textReport statInfo'
   --htmlRpt = htmlReport  quotes statInfo' 




-- you can use this 
readData :: FilePath -> IO [CovidData]
readData fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (V.toList quotes)
    
