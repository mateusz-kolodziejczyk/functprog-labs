{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)
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
    daysBetweenMinMax :: Int
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
    cFieldStatInfo cField =
      let
        get = field2fun cField
        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays get covidData
        decPlaces = decimalPlacesFloating
        meanVal = StatValue decPlaces (mean $ fmap get covidData)
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx
      in StatEntry {..}

instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

instance Buildable StatEntry where
  build StatEntry {..} =
           ""+||cField||+": "
             +|meanVal|+" (mean), "
             +|minVal|+" (min), "
             +|maxVal|+" (max), "
             +|daysBetweenMinMax|+" (days)"

textReport :: [StatEntry] -> String
textReport = ascii colStats
  where
    colStats = mconcat
      [ headed "Covid Data Field" (show . cField)
      , headed "Mean" (pretty . meanVal)
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      ]


