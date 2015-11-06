{-# LANGUAGE OverloadedStrings #-}

import Hash

main :: IO ()
main = createHash (Password "hello") >>= print
