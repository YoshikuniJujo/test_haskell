{-# LANGUAGE ImportQualifiedPost #-}

module TryDecodeToHex where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment
import Codec.Binary.Bech32
import Numeric

home :: IO FilePath
home = getEnv "HOME"

npub :: IO T.Text
npub = do
	hm <- home
	T.readFile $ hm ++ "/tmp/npub"

npub' = decode <$> npub

npubBytes = do
	Right (_, dp) <- npub'
	pure $ dataPartToBytes dp

word8ToHex w = replicate (2 - length s) '0' ++ s
	where s = showHex w ""

toHex = concat . (word8ToHex <$>) . BS.unpack
