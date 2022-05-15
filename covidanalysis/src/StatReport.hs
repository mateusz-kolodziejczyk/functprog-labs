{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy, toList)
import Data.Time (diffDays, Day)
import Data.List (tails)
import Fmt
import Colonnade

import CovidData

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue {
    decimalPlaces :: Int,
    value :: Double
  }

data StatEntry = StatEntry {
    cField :: CField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int,
    maxMAvg :: StatValue,
    meanMAvg :: StatValue
  }

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays :: (Ord a, Foldable t) =>
                     (CovidData -> a) -> t CovidData -> (a, a, Int)
computeMinMaxDays get covidData = (get min, get max, days)
  where
    cmp = comparing get
    min = minimumBy cmp covidData
    max = maximumBy cmp covidData
    days = fromIntegral $ abs $ diffDays (date min) (date max)

statInfo :: (Functor t, Foldable t) => t CovidData -> [StatEntry]
statInfo covidData = fmap cFieldStatInfo [minBound .. maxBound]
  where
    -- Everything besides the reproduction rate deals with logical int values
    calculateDecPlaces ReproductionRate = decimalPlacesFloating
    calculateDecPlaces _ = 0

    cFieldStatInfo cField =
      let

        get = field2fun cField

        -- Use currying to reduce repetition
        getMavg = mavg 10 get
        covidList = toList covidData

        -- Only calculate moving average for valid fields
        calculateMavg NewCases = getMavg covidList
        calculateMavg NewDeaths = getMavg covidList
        calculateMavg ICUPatients = getMavg covidList
        calculateMavg NewVaccinations = getMavg covidList
        calculateMavg _ = [0]

        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays get covidData
        decPlaces = calculateDecPlaces cField
        meanVal = StatValue decPlaces (mean $ fmap get covidData)
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx

        -- List of all 10 day moving averages
        movingAverages = calculateMavg cField
        maxMAvg = StatValue decimalPlacesFloating (maximum movingAverages)
        meanMAvg = StatValue decimalPlacesFloating (mean movingAverages)
        
      in StatEntry {..}

getDateRange :: (Foldable t) => (Day, Day) -> t CovidData -> [CovidData]
getDateRange dates covidData = filter (\x -> date x >= fst dates && date x <= snd dates) (toList covidData)

instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

instance Buildable StatEntry where
  build StatEntry {..} =
           ""+||cField||+": "
             +|meanVal|+" (mean), "
             +|minVal|+" (min), "
             +|maxVal|+" (max), "
             +|daysBetweenMinMax|+" (days),"
             +|maxMAvg|+" (maxmavg),"
             +|meanMAvg|+" (maxmavg),"

textReport :: [StatEntry] -> String
textReport = ascii colStats
  where
    colStats = mconcat
      [ headed "Covid Data Field" (show . cField)
      , headed "Mean" (pretty . meanVal)
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      , headed "Max 10 day average" (pretty . maxMAvg)
      , headed "Mean 10 day average" (pretty . meanMAvg)
      ]

-- Calculates moving avruage using a list, taken from https://stackoverflow.com/a/41353651
mavg :: Int -> (CovidData -> Double) -> [CovidData] -> [Double]
mavg k get covidData = take (length covidData-(k-1)) $ map average $ tails $ map get covidData
   where average = (/ fromIntegral k) . sum . take k