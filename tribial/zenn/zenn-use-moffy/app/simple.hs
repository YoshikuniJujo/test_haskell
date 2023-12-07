{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (repeat, break)
import Control.Monad
import Control.Moffy
import Control.Moffy.Run
import Control.Exception (bracket)
import Data.Type.Set
import Data.Type.Flip
import Data.OneOrMoreApp
import Data.Bool
import Data.Char
import System.IO qualified as IO

data Key = KeyReq deriving (Show, Eq, Ord)
numbered [t| Key |]
instance Request Key where data Occurred Key = OccKey Char deriving Show

key :: React s (Singleton Key) Char
key = await KeyReq \(OccKey c) -> c

main :: IO ()
main = do
	void . withNoBuffering IO.stdin . interpret handle output
		$ ((+) <$%> sigX 'a' <*%> sigX 'b') `break` pressOn 'q'
	putStrLn ""

pressOn :: Char -> React s (Singleton Key) ()
pressOn c = key >>= bool (pressOn c) (pure ()) . (c ==)

digit :: React s (Singleton Key) Int
digit = do
	c <- key
	bool digit (pure . read $ c : "") (isDigit c)

sigX :: Char -> Sig s (Singleton Key) Int ()
sigX c = repeat $ pressOn c >> digit

output :: Int -> IO ()
output c = putStrLn $ "\na + b = " ++ show c

--

handle :: Handle IO (Singleton Key)
handle = const $ Singleton . OccKey <$> getChar

withNoBuffering :: IO.Handle -> IO a -> IO a
withNoBuffering h act = bracket
	(IO.hGetBuffering h <* IO.hSetBuffering h IO.NoBuffering)
	(IO.hSetBuffering h)
	(const act)
