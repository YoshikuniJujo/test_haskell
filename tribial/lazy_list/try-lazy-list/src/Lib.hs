{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Time
import System.IO.Unsafe

getTimes :: IO [UTCTime]
getTimes = unsafeInterleaveIO $ (:) <$> getCurrentTime <*> getTimes

times :: [UTCTime]
times = unsafePerformIO getTimes

times' :: [UTCTime]
times' = unsafePerformIO getCurrentTime : times'
