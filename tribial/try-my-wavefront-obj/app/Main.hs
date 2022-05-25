module Main where

import Codec.Wavefront.Read

main :: IO ()
main = print . readVertices =<< readSample
