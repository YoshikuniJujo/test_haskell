{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

power :: (?int :: b, ?dbl :: a, Num a, Integral b) => a
power = ?dbl ^ ?int

times :: Int -> String -> String
times n str = concat $ replicate n str

nTimes :: (?n :: Int) => String -> String
nTimes = times ?n

greeting :: (?name :: String) => IO ()
greeting = putStrLn $ "Hello, " ++ ?name ++ "!"

foo :: IO ()
foo = do
	let	?name = "Yoshio"
	greeting
