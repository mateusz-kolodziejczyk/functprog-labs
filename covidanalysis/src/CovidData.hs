{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module CovidData where
import qualified Data.Text as T
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, runParser, FromField (..))
import qualified Data.ByteString as BL

data CovidData = CovidData {
                   date :: Day,
                   total_cases :: Double,
                   new_cases :: Double,
                   total_deaths :: DefaultToZero,
                   new_deaths :: DefaultToZero,
                   reproduction_rate :: DefaultToZero,
                   icu_patients :: DefaultToZero,
                   total_vaccinations :: DefaultToZero,
                   people_vaccinated :: DefaultToZero,
                   people_fully_vaccinated :: DefaultToZero,
                   new_vaccinations  :: DefaultToZero
                 }
  deriving (Generic, FromNamedRecord, Show)


-- Some fields are null so they must be able to be parsed
newtype DefaultToZero = DefaultToZero Double
  deriving (Show, Eq, Ord)

instance FromField DefaultToZero where
  parseField s = case runParser (parseField s) of
    Left err -> pure $ DefaultToZero 0
    Right n -> pure $ DefaultToZero n

instance FromField Day where
   parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

data CField = TotalCases | NewCases | TotalDeaths | NewDeaths | ReproductionRate | ICUPatients |
  TotalVaccinations | PeopleVaccinated | PeopleFullyVaccinated | NewVaccinations
  deriving (Eq, Ord, Show, Enum, Bounded)


parseDTZ :: DefaultToZero -> Double
parseDTZ (DefaultToZero d) = d

field2fun :: CField -> CovidData -> Double
field2fun TotalCases = total_cases
field2fun NewCases = new_cases
field2fun TotalDeaths = parseDTZ . total_deaths
field2fun NewDeaths = parseDTZ . new_deaths
field2fun ReproductionRate = parseDTZ . reproduction_rate
field2fun ICUPatients = parseDTZ . icu_patients
field2fun TotalVaccinations = parseDTZ . total_vaccinations
field2fun PeopleVaccinated = parseDTZ . people_vaccinated
field2fun PeopleFullyVaccinated = parseDTZ . people_fully_vaccinated
field2fun NewVaccinations = parseDTZ . new_vaccinations

