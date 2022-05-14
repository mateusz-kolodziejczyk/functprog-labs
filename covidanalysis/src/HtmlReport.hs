{-# LANGUAGE OverloadedStrings #-}

module HtmlReport where

import Data.Foldable (traverse_)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Colonnade
import Text.Blaze.Colonnade
import Fmt (pretty, Buildable)

import CovidData
import StatReport

viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty

colStats :: Colonnade Headed StatEntry Html
colStats = mconcat
      [ headed "Covid Data Field" (i . string . show . cField)
      , headed "Mean" (viaFmt . meanVal)
      , headed "Min" (viaFmt . minVal)
      , headed "Max" (viaFmt . maxVal)
      , headed "Days between Min/Max" (viaFmt . daysBetweenMinMax)
      ]

                  --  date :: Day,
                  --  total_cases :: Double,
                  --  new_cases :: Double,
                  --  total_deaths :: DefaultToZero,
                  --  new_deaths :: DefaultToZero,
                  --  reproduction_rate :: DefaultToZero,
                  --  icu_patients :: DefaultToZero,
                  --  total_vaccinations :: DefaultToZero,
                  --  people_vaccinated :: DefaultToZero,
                  --  people_fully_vaccinated :: DefaultToZero,
                  --  new_vaccinations  :: DefaultToZero
colData :: Colonnade Headed CovidData Html
colData = mconcat
      [ headed "Date" (viaFmt . date),
        headed "Total Cases" (viaFmt . total_cases),
        headed "New Cases" (viaFmt . new_cases),    
        headed "Total Deaths" (viaFmt . parseDTZ . total_deaths),
        headed "New Deaths" (viaFmt . parseDTZ . new_deaths),
        headed "Reproduction Rate" (viaFmt . parseDTZ . reproduction_rate),
        headed "ICU Patients" (viaFmt . parseDTZ . icu_patients),
        headed "Total Vaccinations" (viaFmt . parseDTZ . total_vaccinations),
        headed "People Vaccinated" (viaFmt . parseDTZ . people_vaccinated),
        headed "People Fully Vaccinated" (viaFmt . parseDTZ . people_fully_vaccinated),
        headed "New Vaccinations" (viaFmt . parseDTZ . new_vaccinations)
      ]

htmlReport :: (Functor t, Foldable t) =>
             t CovidData -> [StatEntry]  -> ByteString
htmlReport covidData statEntries  = renderHtml $ docTypeHtml $ do
     H.head $ do
       style tableStyle

     body $ do
      
       h1 "Statistics Report"
       encodeHtmlTable mempty colStats statEntries

       h1 "Covid Data Data"
       encodeHtmlTable mempty colData covidData
  where
    tableStyle = "table {border-collapse: collapse}" <>
            "td, th {border: 1px solid black; padding: 5px}"
