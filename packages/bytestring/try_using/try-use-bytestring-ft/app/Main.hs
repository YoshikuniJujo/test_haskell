{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.ToolsYj
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Char8 qualified as BSFC
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	BS.putStr . BSF.toStrict =<< doWhile BSF.Empty \bsf -> do
		eof <- hIsEOF h
		bs <- bool (BSC.hGetLine h) (pure "") eof
		pure (not eof, bsf BSF.:|> BSC.singleton '\n' BSF.:|> bs)
