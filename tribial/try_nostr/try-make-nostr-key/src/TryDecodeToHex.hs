{-# LANGUAGE ImportQualifiedPost #-}

module TryDecodeToHex where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment
import Codec.Binary.Bech32

home :: IO FilePath
home = getEnv "HOME"

npub :: IO T.Text
npub = do
	hm <- home
	T.readFile $ hm ++ "/tmp/npub"
