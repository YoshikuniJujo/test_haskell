{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.IO.Yj (withNoEcho) where

import Control.Exception
import System.IO

withNoEcho :: IO a -> IO a
withNoEcho = bracket
	(hGetEcho stdin <* hSetEcho stdin False) (hSetEcho stdin) . const
