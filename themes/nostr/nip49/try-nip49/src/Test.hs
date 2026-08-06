{-# LANGUAGE ImportQualifiedPost #-}

module Test where

import Control.Monad
import Data.Text.IO qualified as T
import System.Environment
import System.FilePath

import Ncryptsec
import Tools

jsdir :: IO FilePath
jsdir = do
	h <- getEnv "PWD"
	pure $ h </>
		"../../../../other_language/javascript/nostr/try-nostr-crypto-utils/test-vectors"

test1 :: FilePath -> String -> Int -> IO ()
test1 dr nm n = do
	foo <- T.readFile $ dr </> fileNameN nm "ncryptsec" n
	r <- toNsec (readFile $ dr </> fileNameN nm "password" n) foo
	r0 <- T.readFile (dr </> fileNameN nm "nsec" n)
	T.putStrLn r0
	T.putStrLn r
	when (r /= r0) $ error "BAD"
