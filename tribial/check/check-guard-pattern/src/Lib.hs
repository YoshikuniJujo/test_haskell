{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Bool

getEven, getOdd :: Integer -> Maybe Integer
getEven n = bool Nothing (Just n) $ even n
getOdd n = bool Nothing (Just n) $ odd n

{-# COMPLETE Even, Odd #-}

pattern Even, Odd :: Integer -> Integer
pattern Even n <- (getEven -> Just n)
pattern Odd n <- (getOdd -> Just n)

showEvenOdd :: Integer -> String
showEvenOdd (Even n) = "Even " ++ show n
showEvenOdd (Odd n) = "Odd " ++ show n
