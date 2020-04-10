{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.TestMonad where

import Prelude hiding (log)

import Control.Monad.Writer
import Control.Monad.State
import Data.Type.Set
import Data.UnionSet

import qualified Data.Text as T

import Trials.Followbox.Event
import MonadicFrp

data Event = LeftClick | HttpResult T.Text deriving Show

type TestMonad a = StateT [Event] (Writer String) a

runTestMonad :: TestMonad a -> [Event] -> ((a, [Event]), String)
m `runTestMonad` es = runWriter $ m `runStateT` es

printTestMonad :: TestMonad a -> [Event] -> IO ()
m `printTestMonad` es = do
	let	(_, l) = m `runTestMonad` es
	putStr l

getEvent :: TestMonad Event
getEvent = get >>= \case e : es -> e <$ put es; [] -> error "no more event"

log :: Show a => a -> TestMonad ()
log = lift . tell . (++ "\n") . show

tryTestMonad :: TestMonad Int
tryTestMonad = do
	e <- getEvent
	log e
	pure 123

testHandleHttpGet :: EvReqs (Singleton HttpGet) -> TestMonad (EvOccs (Singleton HttpGet))
testHandleHttpGet reqs = do
	log u
	pure . singleton $ OccHttpGet u [] "foobar"
	where
	HttpGetReq u = extract reqs
