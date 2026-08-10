{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.ByteString qualified as BS
import System.Random qualified as R
import System.FilePath

fileNameN :: String -> String -> Int -> FilePath
fileNameN b e n = b ++ replicate (2 - length s) '0' ++ s <.> e where s = show n

password :: State R.StdGen String
password = (`replicateM` select chars) =<< state (R.randomR (5, 30))
	where
	select xs = state $ ((xs !!) `first`) . R.randomR (0, length xs - 1)

chars :: [Char]
chars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++
	['!' .. '/'] ++ [':' .. '@'] ++ ['[' .. '`'] ++ ['{' .. '~']

splitAtR :: Int -> BS.ByteString -> (BS.ByteString, BS.ByteString)
splitAtR n bs = BS.splitAt (BS.length bs - n) bs
