{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)
import qualified Data.Vector as V
import Data.Time (Day)
import CovidData
import StatReport
import HtmlReport
import Util

main ::  IO ()
main  = do
  putStrLn "Please enter the filepath/name of the csv file:"
  fname <- getLine 
  putStrLn "Please enter the name of the html target file"
  htmlFile <- getLine
  putStrLn "Please enter the first date(YYYY-MM-DD)"
  d1 <- getLine
  putStrLn "Please enter the second date(YYYY-MM-DD)"
  d2 <- getLine

  let dateList = map parseDay [d1,d2]
  let dates = (minimum dateList, maximum dateList)
  covidData <- readData fname
  generateReports htmlFile covidData dates


generateReports :: (Functor t, Foldable t) =>
                 String -> t CovidData -> (Day, Day) -> IO ()
generateReports htmlFl covidData dates = do
  putStrLn "Overall Data"
  putStrLn textRpt
  putStrLn ("Data between " ++ show (fst dates) ++ " and " ++ show (snd dates))
  putStrLn rangeRpt
  BL.writeFile htmlFl htmlRpt
 where
   statInfo' = statInfo covidData
   rangeStatInfo = statInfo (getDateRange dates covidData)
   textRpt = textReport statInfo'
   rangeRpt = textReport rangeStatInfo
   htmlRpt = htmlReport covidData statInfo' 




-- you can use this 
readData :: FilePath -> IO [CovidData]
readData fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, covidData) -> pure (V.toList covidData)
    
