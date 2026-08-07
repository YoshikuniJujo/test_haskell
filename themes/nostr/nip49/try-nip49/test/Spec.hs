{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.State
import System.Exit
import System.Entropy
import System.Random qualified as R

import Ncryptsec
import Codec.Bech32 qualified as Bech32
import Codec.Bech32.ByteString qualified as Bech32.BS

import Tools

main :: IO ()
main = do
	putStrLn "Only 1 test suite is implemented"
	sk <- getEntropy 32
	let	ns = Bech32.encode $ Bech32.BS.decode "nsec" sk
		(pss, _) = password `runState` R.mkStdGen 16
	ncs <- fromNsec 16 0 (pure pss) ns
	ns' <- toNsec (pure pss) ncs
	if ns' == ns then exitSuccess else exitFailure
