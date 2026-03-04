{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Nostr (

	sample1,
	sampleFilter1, sampleFilterUntilEose, sampleFilterPair,

	authorFilter, kindFilter

	) where

import Prelude hiding (break, scanl)
import Control.Monad
import Control.Moffy
import Control.Moffy.Nostr.Event
import Control.Moffy.Nostr.Run
import Data.Type.Flip
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Nostr.Event qualified as Event
import Nostr.Filter qualified as Filter

sample1 :: IO ()
sample1 = sampleFilter1
	nullFilter { Filter.kinds = Just [1], Filter.limit = Just 5 }

sampleFilter1 :: Filter.Filter -> IO ()
sampleFilter1 flt = run (const print) "nos.lol" "443" do
	waitFor $ request "req-1" flt
	emit . snd =<< waitFor awaitEvent
	waitFor halt

sampleFilterUntilEose :: Filter.Filter -> IO ()
sampleFilterUntilEose flt = run (const printEvent) "nos.lol" "443" do
	waitFor $ request "req-2" flt
	void $ forever (emitEv "req-2") `break` awaitNameEose "req-2"
	waitFor halt

sampleFilterPair :: Filter.Filter -> Filter.Filter -> IO ()
sampleFilterPair flt1 flt2 = run (const print2Req) "nos.lol" "443" do
	waitFor $ request "req-3" flt1 >> request "req-4" flt2
	void $ ((,)
		<$%> scanl (flip (:)) [] (forever $ emitEv "req-3")
		<*%> scanl (flip (:)) [] (forever @_ @_ @() $ emitEv "req-4"))
		`break` ((\_ _ -> ())
			<$> awaitNameEose "req-3"
			<*> awaitNameEose "req-4")
	waitFor halt

printEvent :: Event.E -> IO ()
printEvent ev = print ev >> T.putStrLn (Event.content ev)

print2Req :: ([Event.E], [Event.E]) -> IO ()
print2Req (evs1, evs2) = printEvent `mapM_` evs1 >> printEvent `mapM_` evs2

emitEv :: T.Text -> Sig s Events Event.E ()
emitEv nm = emit =<< waitFor (awaitNameEvent nm)

nullFilter :: Filter.Filter
nullFilter = Filter.Filter {
	Filter.ids = Nothing, Filter.authors = Nothing,
	Filter.kinds = Just [1], Filter.tags = [],
	Filter.since = Nothing, Filter.until = Nothing, Filter.limit = Nothing }

authorFilter :: FilePath -> IO Filter.Filter
authorFilter fp = do
	Right pk <- Event.publicFromBech32 <$> T.readFile fp
	pure nullFilter {
		Filter.authors = Just [pk],
		Filter.kinds = Just [1], Filter.limit = Just 5 }

kindFilter :: Int -> Filter.Filter
kindFilter k = nullFilter { Filter.kinds = Just [k], Filter.limit = Just 5 }
