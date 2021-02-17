{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import System.IO

getPassword :: IO String
getPassword = do
	e <- hGetEcho stdin
	hSetEcho stdin False
	myGetLine <* hSetEcho stdin e

myGetLine :: IO String
myGetLine = getChar >>= \case '\n' -> pure ""; c -> (c :) <$> myGetLine
