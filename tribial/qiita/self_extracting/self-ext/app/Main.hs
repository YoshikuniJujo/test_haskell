{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString as BS
import Text.Nowdoc

import Lib

main :: IO ()
main = BS.writeFile "some.jpg" cat

cat :: ByteString
cat = [binfile|cat.bmp|]
