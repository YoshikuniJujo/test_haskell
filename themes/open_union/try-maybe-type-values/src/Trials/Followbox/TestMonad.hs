{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.TestMonad where

import Prelude hiding (log)

import Control.Arrow
import Control.Monad.Writer
import Control.Monad.State
import Data.Type.Set
import Data.UnionSet

import qualified Data.Text as T

import Trials.Followbox.Event
import Trials.Followbox.Aeson
import MonadicFrp hiding (first)

data Event = LeftClick | HttpResult T.Text deriving Show

type TestMonad = StateT ([Event], [Object]) (Writer String)

runTestMonad :: TestMonad a -> [Event] -> ((a, [Event]), String)
m `runTestMonad` es = second fst `first` runWriter (m `runStateT` (es, []))

printTestMonad :: TestMonad a -> [Event] -> IO ()
m `printTestMonad` es = do
	let	(_, l) = m `runTestMonad` es
	putStr l

getEvent :: TestMonad Event
getEvent = get >>= \case
	(e : es, os) -> e <$ put (es, os)
	([], _) -> error "no more event"

putJsons :: [Object] -> TestMonad ()
putJsons os = do
	(es, _os) <- get
	put (es, os)

getJsons :: TestMonad [Object]
getJsons = snd <$> get

log :: String -> TestMonad ()
log = lift . tell . (++ "\n")

tryTestMonad :: TestMonad Int
tryTestMonad = do
	e <- getEvent
	log $ show e
	pure 123

testHandleHttpGet :: EvReqs (Singleton HttpGet) -> TestMonad (EvOccs (Singleton HttpGet))
testHandleHttpGet reqs = do
	log $ "HTTP GET " <> u
	pure . singleton $ OccHttpGet u [] "[{\"foo\": \"bar\"}]"
--	pure . singleton $ OccHttpGet u [] "[]"
	where HttpGetReq u = extract reqs

testHandleStoreJsons :: Handle TestMonad (Singleton StoreJsons)
testHandleStoreJsons reqs = do
	putJsons os
	pure . singleton $ OccStoreJsons os
	where StoreJsons os = extract reqs

testHandleLoadJsons :: Handle TestMonad (Singleton LoadJsons)
testHandleLoadJsons _reqs = singleton . OccLoadJsons <$> getJsons

testHandleLeftClick :: Handle TestMonad (LeftClick :- Quit :- 'Nil)
testHandleLeftClick _reqs = pure . expand $ singleton OccLeftClick

testHandleRaiseError :: Handle' TestMonad (Singleton RaiseError)
testHandleRaiseError reqs = do
	log $ "ERROR: " <> em
	case e of
		NotJson -> pure . Just . singleton $ OccRaiseError e Terminate
		NoLoginName -> pure . Just . singleton $ OccRaiseError e Terminate
		CatchError -> pure Nothing
	where RaiseError e em = extract reqs

testHandle :: Handle TestMonad FollowboxEv
testHandle = retryHandle $
	(Just <$>) . testHandleHttpGet `mergeHandle`
	(Just <$>) . testHandleStoreJsons `mergeHandle`
	(Just <$>) . testHandleLoadJsons `mergeHandle`
	(Just <$>) . testHandleLeftClick `mergeHandle` testHandleRaiseError
