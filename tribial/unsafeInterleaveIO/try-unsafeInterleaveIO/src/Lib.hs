module Lib where

import Data.Traversable
import Data.Time
import System.IO.Unsafe

timesStrict :: IO [UTCTime]
timesStrict = for [0 .. 5] $ const getCurrentTime

forLazy :: [a] -> (a -> IO b) -> IO [b]
forLazy [] f = pure []
forLazy (x : xs) f = unsafeInterleaveIO $ (:) <$> f x <*> forLazy xs f

timesLazy :: IO [UTCTime]
timesLazy = forLazy [0 .. 5] $ const getCurrentTime

timesLazyLazy :: IO [UTCTime]
timesLazyLazy = forLazy [0 .. 5] . const $ unsafeInterleaveIO getCurrentTime
