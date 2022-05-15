module Util where
import Data.Time (Day, parseTimeOrError, defaultTimeLocale)

parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"