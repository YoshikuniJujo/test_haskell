{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StackedInterpreter where

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

data SigValue = SigValueReq deriving (Show, Eq, Ord)
numbered [t| SigValue |]

instance Request SigValue where
	data Occurred SigValue = OccSigValue Int deriving Show

sigValue :: React s (Singleton SigValue) Int
sigValue = await SigValueReq \(OccSigValue i) -> i

---------------------------------------------------------------------------

type StackedSig s = Sig s (SigValue :- Singleton Key)

handleStacked :: forall s . StackedSig s Int Int -> Handle (StackedSig s Int) (SigValue :- Singleton Key)
handleStacked sig = const $ expand . Singleton . OccSigValue <$> (interpret (handleStacked sig) outputStacked sig :: StackedSig s Int Int)

outputStacked :: Int -> StackedSig s Int ()
outputStacked i = emit i

---------------------------------------------------------------------------

main :: IO ()
main = withNoBuffering IO.stdin run >> putStrLn ""

run :: IO ()
run = interpret handle output sigC

sigC :: Sig s (Singleton Key) Int ()
sigC = void $ ((+) <$%> sigX 'a' <*%> sigX 'b') `break` pressOn 'q'

sigX :: Char -> Sig s (Singleton Key) Int ()
sigX c = repeat $ pressOn c >> digit

pressOn :: Char -> React s (Singleton Key) ()
pressOn c = key >>= bool (pressOn c) (pure ()) . (c ==)

digit :: React s (Singleton Key) Int
digit = key >>= \c -> bool digit (pure . read $ c : "") (isDigit c)

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
