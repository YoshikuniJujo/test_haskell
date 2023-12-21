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

---------------------------------------------------------------------------

pressOn :: Char -> React s (Singleton Key) ()
pressOn c = key >>= bool (pressOn c) (pure ()) . (c ==)

digit :: React s (Singleton Key) Int
digit = key >>= \c -> bool digit (pure . read $ c : "") (isDigit c)

sigX :: Char -> Sig s (Singleton Key) Int ()
sigX c = repeat $ pressOn c >> digit

sigC :: Sig s (Singleton Key) Int ()
sigC = void $ ((+) <$%> sigX 'a' <*%> sigX 'b') `break` pressOn 'q'

---------------------------------------------------------------------------

handle :: Handle IO (Singleton Key)
handle = const $ Singleton . OccKey <$> getChar

output :: Int -> IO ()
output c = putStrLn $ "\na + b = " ++ show c

withNoBuffering :: IO.Handle -> IO a -> IO a
withNoBuffering h act = bracket
	(IO.hGetBuffering h <* IO.hSetBuffering h IO.NoBuffering)
	(IO.hSetBuffering h)
	(const act)

main :: IO ()
main = withNoBuffering IO.stdin run >> putStrLn ""

run :: IO ()
run = interpret handle output sigC
