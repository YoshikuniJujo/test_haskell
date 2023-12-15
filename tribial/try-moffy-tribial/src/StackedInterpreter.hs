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
import Control.Moffy.Handle
import Control.Moffy.Run
import Control.Exception (bracket)
import Data.Type.Set
import Data.Type.Flip
import Data.OneOrMore qualified as Oom
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

fahrenheit :: Sig s (Singleton Key) Double ()
fahrenheit = interpret (retry handleStacked) emit celsius

celsius :: Sig s (Singleton Key) Double ()
celsius = interpret (retry handleStacked) emit fahrenheit

---------------------------------------------------------------------------

type StackedSig s = Sig s (Singleton Key)

handleStacked :: Handle' (StackedSig s a) (Singleton Key)
handleStacked req = case Oom.project req of
	Just krq -> Just <$> waitFor (await krq Singleton)
	Nothing -> pure Nothing

outputStacked :: Int -> StackedSig s Int ()
outputStacked i = (1 +) <$%> emit i

sigFoo :: Sig s (Singleton Key) Int ()
sigFoo = emit 0 >> waitFor (pressOn ' ')
	>> interpret (retry handleStacked) outputStacked sigFoo

sigFib0, sigFib0' :: Sig s (Singleton Key) Int ()
sigFib0 = emit 0 >> waitFor (pressOn ' ') >> sigFib1
sigFib0' = emit 0 >> waitFor (pressOn ' ') >> sigFib1'

sigFib1, sigFib1' :: Sig s (Singleton Key) Int ()
sigFib1 = do
	emit 1 >> waitFor (pressOn ' ')
	(+)	<$%> interpret (retry handleStacked) emit sigFib0
		<*%> interpret (retry handleStacked) emit sigFib1
sigFib1' = do
	emit 1 >> waitFor (pressOn ' ')
	(+) <$%> sigFib0' <*%> sigFib1'

mainSigFoo :: IO ()
mainSigFoo = withNoBuffering IO.stdin runSigFoo >> putStrLn ""

runSigFoo :: IO ()
-- runSigFoo = void $ interpret handle output (sigFoo `break` pressOn 'q')
-- runSigFoo = void $ interpret handle output (sigFib0 `break` pressOn 'q')
runSigFoo = void $ interpret handle output (sigFib0' `break` pressOn 'q')

---------------------------------------------------------------------------

main' :: IO ()
main' = withNoBuffering IO.stdin run >> putStrLn ""

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
