import Control.Applicative
import Data.Time
import Data.Time.Format
import System.Locale

format :: String
format = "%a, %d-%b-%Y %H:%M:%S GMT"

getCurrentFormatted :: IO String
getCurrentFormatted = formatTime defaultTimeLocale format <$> getCurrentTime

parseFormatted :: String -> Maybe UTCTime
parseFormatted = parseTime defaultTimeLocale format
