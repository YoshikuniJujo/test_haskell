{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Test where

import Control.Monad
import Data.Text.IO qualified as T
import System.Environment
import System.FilePath

import Ncryptsec
import Tools

jsdir :: IO FilePath
jsdir = (<$> getEnv "PWD") \h -> joinPath [
	h, "..", "..", "..", "..", "other_language",
	"javascript", "nostr", "try-nostr-crypto-utils", "test-vectors" ]

webextdir :: IO FilePath
webextdir = (<$> getEnv "PWD") \h -> joinPath [
	h, "..", "..", "..", "..", "other_language",
	"web_extensions", "try-nip49", "test_vectors" ]

test1 :: FilePath -> String -> Int -> IO ()
test1 dr nm n = do
	ns0 <- T.readFile (dr </> fileNameN nm "nsec" n)
	ncs <- T.readFile $ dr </> fileNameN nm "ncryptsec" n
	ns1 <- toNsec (readFile $ dr </> fileNameN nm "password" n) ncs
	T.putStrLn ns0; T.putStrLn ns1
	when (ns1 /= ns0) $ error "BAD"
