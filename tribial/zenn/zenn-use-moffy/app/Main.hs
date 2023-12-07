{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Moffy
import Data.Type.Set

import Control.Moffy.Run
import Data.OneOrMoreApp

data Key = KeyReq deriving (Show, Eq, Ord)
numbered [t| Key |]
instance Request Key where data Occurred Key = OccKey Char deriving Show

main :: IO ()
main = print =<< foo

key :: React s (Singleton Key) Char
key = await KeyReq \(OccKey c) -> c

foo :: IO Char
foo = interpretReact
	(const $ Singleton . OccKey <$> getChar :: Handle IO (Singleton Key))
	$ key >> key
