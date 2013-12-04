{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BSLC

testData :: BSLC.ByteString
testData = BSLC.concat $ replicate (2 * 10 ^ 7) "1234567890"

main :: IO ()
main = BSLC.writeFile "moreBig2.txt" testData
