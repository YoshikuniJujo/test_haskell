{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.NostrBak (
	sample, sample', sampleFilter, sampleFilter', sampleFilter2,
	sampleId, sampleIdT, sampleId'
	) where

import Prelude hiding (break, scanl)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Handle qualified as Handle
import Control.Moffy.Run
import Control.Moffy.Event.ThreadId
import Control.Moffy.Handle.ThreadId
import Control.Moffy.Samples.Event.Random qualified as Rnd
import Control.Moffy.Samples.Handle.TChan qualified as TC
import Control.Moffy.Samples.Handle.Random qualified as Rnd
import Data.Type.Flip
import Data.Type.Set
import Data.Word
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Random
import Nostr.Event qualified as Event
import Nostr.Filter qualified as Filter
import Wuss qualified as Ws

import Tools
import TryBech32

import Control.Moffy.Nostr.Event
import Control.Moffy.Nostr.Run

type EventsThread = Events :+: (GetThreadId :- 'Nil)

type EventsRnd = Req :- Event :- Eose :- Halt :- End :- Rnd.RandomEv

sample :: IO ()
sample = run (const print) "nos.lol" "443" $ do
	_ <- waitFor . adjust $ await (ReqReq "foobar" Filter.Filter {
		Filter.ids = Nothing, Filter.authors = Nothing,
		Filter.kinds = Just [1], Filter.tags = [],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }) id
	emit =<< waitFor (adjust $ await EventReq \(OccEvent _nm ev) -> ev)
	waitFor . adjust $ await HaltReq id

sample' :: IO ()
sample' = run' print "nos.lol" "443" $ do
	_ <- waitFor . adjust $ await (ReqReq "foobar" Filter.Filter {
		Filter.ids = Nothing, Filter.authors = Nothing,
		Filter.kinds = Just [1], Filter.tags = [],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }) id
	emit =<< waitFor (adjust $ await EventReq \(OccEvent _nm ev) -> ev)
	waitFor . adjust $ await HaltReq id

sampleFilter :: Filter.Filter -> IO ()
sampleFilter flt = run (const print) "nos.lol" "443" $ do
	_ <- waitFor . adjust $ await (ReqReq "foobar" flt) id
	emit =<< waitFor (adjust $ await EventReq \(OccEvent _nm ev) -> ev)
	waitFor . adjust $ await HaltReq id

sampleFilter' :: Filter.Filter -> IO ()
sampleFilter' flt = run (const printEvent) "nos.lol" "443" do
	waitFor $ request "foobar" flt
	_ <- untilEose "foobar" `break` awaitNameEose "foobar"
	waitFor . adjust $ await HaltReq (const ())

sampleFilter2 :: Filter.Filter -> Filter.Filter -> IO ()
sampleFilter2 flt1 flt2 = run (const print2Req) "nos.lol" "443" do
	waitFor $ request "foobar" flt1
	waitFor $ request "hogepiyo" flt2
	_ <- ((,)
		<$%> scanl (flip (:)) [] (untilEose "foobar")
		<*%> scanl (flip (:)) [] (untilEose "hogepiyo"))
		`break` ((\_ _ -> ()) <$> awaitNameEose "foobar" <*> awaitNameEose "hogepiyo")
	waitFor . adjust $ await HaltReq (const ())

sampleId :: FilePath -> IO ()
sampleId fp = do
	Just flt <- filterP <$> T.readFile fp
	run (\_ ms -> do
			print (length ms)
			zipWithM_ printEventE' [0 ..] ms
			atomically case ms of
				[] -> pure () -- writeTVar end True
				_ -> pure ()
			) "nos.lol" "443" do
		waitFor (request "foobar" flt)
		_ <- readingEventE `break` await EndReq (const ())
		waitFor . adjust $ await HaltReq (const ())

sampleIdT :: FilePath -> IO ()
sampleIdT fp = do
	Just flt <- filterP <$> T.readFile fp
	run'' (printEventE `mapM_`) "nos.lol" "443" do
		waitFor . adjust $ request "foobar" flt
		readingEventET
		waitFor . adjust $ await HaltReq (const ())

sampleId' :: FilePath -> IO ()
sampleId' fp = do
	Just flt <- filterP <$> T.readFile fp
	run' (printEventE `mapM_`) "nos.lol" "443" do
		waitFor . adjust $ request "foobar" flt
		readingEventE'
		waitFor . adjust $ await HaltReq (const ())

printEvent :: Event.E -> IO ()
printEvent ev = do
	print ev
	T.putStrLn $ Event.content ev

printEventE :: (Event.E, Maybe Event.E) -> IO ()
printEventE (ev, mev) = do
	print ev
	T.putStrLn $ Event.content ev
	print $ lookupE ev
	case mev of
		Nothing -> pure ()
		Just ev' -> printEvent ev'

printEventE' :: Int -> (Event.E, Maybe Event.E) -> IO ()
printEventE' n (ev, mev) = do
	putStrLn $ "*** " ++ show n ++ "***"
	print ev
	T.putStrLn $ Event.content ev
	print $ lookupE ev
	case mev of
		Nothing -> putStrLn "NO ROOT MESSAGE"
		Just ev' -> printEvent ev'

lookupE :: Event.E -> Maybe T.Text
lookupE = (fst <$>) . lookup "e" . Event.tags

filterId :: T.Text -> Filter.Filter
filterId a = Filter.Filter {
	Filter.ids = Just [fromHex a],
	Filter.authors = Nothing, Filter.kinds = Just [1],
	Filter.tags = [],
	Filter.since = Nothing, Filter.until = Nothing,
	Filter.limit = Just 1 }

print2Req :: ([Event.E], [Event.E]) -> IO ()
print2Req (evs1, evs2) = do
	printEvent `mapM_` evs1
	printEvent `mapM_` evs2

untilEose :: T.Text -> Sig s Events Event.E ()
untilEose nm = do
	void $ (forever $ emit =<< (waitFor $ awaitNameEvent nm)) -- `break` awaitNameEose nm

readingEventE :: Sig s Events [(Event.E, Maybe Event.E)] ()
readingEventE = do
	void . parList . spawn $ awaitNameEventESig "foobar"

readingEventET :: Sig s EventsThread [(Event.E, Maybe Event.E)] ()
readingEventET = do
	void . parList . spawn $ emit =<< (waitFor $ awaitNameEventET "foobar")

readingEventE' :: Sig s EventsRnd [(Event.E, Maybe Event.E)] ()
readingEventE' = do
	void . parList . spawn $ emit =<< (waitFor $ awaitNameEventE' "foobar")

awaitNameEventESig :: T.Text -> Sig s Events (Event.E, Maybe Event.E) ()
awaitNameEventESig nm0 = do
	ev <- waitFor $ awaitNameEvent nm0
	case lookupE ev of
		Nothing -> emit (ev, Nothing)
		Just e -> do
			let	nm = "bar" <> e
			waitFor $ request nm (filterId e)
			emit (ev, Nothing)
			ev' <- waitFor $ awaitNameEvent nm
			emit (ev, Just ev')

awaitNameEventET :: T.Text -> React s EventsThread (Event.E, Maybe Event.E)
awaitNameEventET nm0 = do
	ev <- adjust $ awaitNameEvent nm0
	case lookupE ev of
		Nothing -> pure (ev, Nothing)
		Just e -> do
			nm <- threadedName
			adjust $ request nm (filterId e)
			ev' <- adjust $ awaitNameEvent nm
			pure (ev, Just ev')

awaitNameEventE' :: T.Text -> React s EventsRnd (Event.E, Maybe Event.E)
awaitNameEventE' nm0 = do
	ev <- adjust $ awaitNameEvent nm0
	case lookupE ev of
		Nothing -> pure (ev, Nothing)
		Just e -> do
			nm <- randomName'
			adjust $ request nm (filterId e)
			ev' <- adjust $ awaitNameEvent nm
			pure (ev, Just ev')

randomName' :: React s EventsRnd T.Text
randomName' = do
	n <- adjust Rnd.getRandom
	pure . T.pack $ "barbarbar" ++ show @Word64 n

threadedName :: React s EventsThread T.Text
threadedName = do
	n <- adjust getThreadId
	pure . T.pack $ "barbarbar" ++ show n

filterP :: T.Text -> Maybe Filter.Filter
filterP a = do
	pub <- toHex <$> dataPart' "npub" (chomp a)
	pure Filter.Filter {
		Filter.ids = Nothing,
		Filter.authors = Nothing, Filter.kinds = Just [1],
		Filter.tags = [('p', [pub])],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }

handle' ::
	Rnd.RandomState st =>
	TChan (EvReqs Events) -> TChan (EvOccs Events) -> HandleSt st IO EventsRnd
handle' cr co = Handle.retrySt
	$ Handle.liftHandle' (TC.handle (Just 0.5) cr co) `Handle.mergeSt` Rnd.handleRandom

handle'' ::
	TChan (EvReqs Events) -> TChan (EvOccs Events) -> Handle IO EventsThread
handle'' cr co = Handle.retry
	$ TC.handle (Just 0.5) cr co `Handle.merge` handleGetThreadId

run' :: (a -> IO ()) -> String -> String -> Sig s EventsRnd a r -> IO ()
run' pr raddr rprt s = do
	ed <- atomically $ newTVar Pre
	cr <- atomically newTChan
	co <- atomically newTChan
	_ <- forkIO $ do
		_ <- interpretSt (handle' cr co) pr s (mkStdGen 8)
		pure ()
	Ws.runSecureClient raddr (read rprt) "/" $ ws cr co ed

run'' :: (a -> IO ()) -> String -> String -> Sig s EventsThread a r -> IO ()
run'' pr raddr rprt s = do
	ed <- atomically $ newTVar Pre
	cr <- atomically newTChan
	co <- atomically newTChan
	_ <- forkIO $ do
		_ <- interpret (handle'' cr co) pr s
		pure ()
	Ws.runSecureClient raddr (read rprt) "/" $ ws cr co ed
