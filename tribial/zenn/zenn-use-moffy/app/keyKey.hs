{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Moffy
import Control.Moffy.Run
import Control.Exception (bracket)
import Data.Type.Set
import Data.OneOrMoreApp
import System.IO qualified as IO

data Key = KeyReq deriving (Show, Eq, Ord)
numbered [t| Key |]
instance Request Key where data Occurred Key = OccKey Char deriving Show

main :: IO ()
main = print =<< foo

foo :: IO Char
foo = withNoBuffering IO.stdin . interpretReact handle $ key >> key

key :: React s (Singleton Key) Char
key = await KeyReq \(OccKey c) -> c

handle :: Handle IO (Singleton Key)
handle = const $ Singleton . OccKey <$> getChar

withNoBuffering :: IO.Handle -> IO a -> IO a
withNoBuffering h act = bracket
	(IO.hGetBuffering h <* IO.hSetBuffering h IO.NoBuffering)
	(IO.hSetBuffering h)
	(const act)
