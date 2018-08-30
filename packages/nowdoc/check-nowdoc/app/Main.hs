{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Nowdoc

main :: IO ()
main = putStr [nowdoc|
I am rock,
I am an island.
And a rock feels no pain,
And an island never cries.
|]
