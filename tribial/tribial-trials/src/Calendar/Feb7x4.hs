module Calendar.Feb7x4 where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

febLength :: Integer -> Int
febLength y = gregorianMonthLength y 2

feb1dayOfWeek :: Integer -> DayOfWeek
feb1dayOfWeek y = dayOfWeek $ fromGregorian y 2 1

feb7x4 :: [Integer]
feb7x4 = [ y | y <- [0 ..], febLength y == 28, feb1dayOfWeek y == Sunday ]
