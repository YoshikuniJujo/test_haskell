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

	authorFilter, kindFilter,
	sinceUntilFilter, japaneseTime, exampleSince, exampleUntil

	) where

import Prelude hiding (break, scanl)
import Foreign.C.Types
import Control.Monad
import Control.Moffy
import Control.Moffy.Nostr.Event
import Control.Moffy.Nostr.Run
import Data.Type.Flip
import Data.Fixed
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.UnixTime
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

zonedToUnixTime :: ZonedTime -> UnixTime
zonedToUnixTime = fromEpochTime
	. CTime . truncate . utcTimeToPOSIXSeconds . zonedTimeToUTC

sinceUntilFilter :: FilePath -> ZonedTime -> ZonedTime -> IO Filter.Filter
sinceUntilFilter fp s u = do
	Right pk <- Event.publicFromBech32 <$> T.readFile fp
	pure nullFilter {
		Filter.authors = Just [pk],
		Filter.kinds = Just [1],
		Filter.since = Just $ zonedToUnixTime s,
		Filter.until = Just $ zonedToUnixTime u,
		Filter.limit = Just 100 }

japaneseTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> ZonedTime
japaneseTime y mt d h m s = ZonedTime
	(LocalTime (fromGregorian y mt d) (TimeOfDay h m s))
	(TimeZone 540 False "JST")

exampleSince, exampleUntil :: ZonedTime
exampleSince = japaneseTime 2025 4 4 12 15 15
exampleUntil = japaneseTime 2025 4 5 0 5 15
