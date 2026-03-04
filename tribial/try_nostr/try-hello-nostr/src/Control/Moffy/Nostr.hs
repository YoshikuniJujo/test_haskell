{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Nostr where

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
import Data.OneOrMore qualified as OOM
import Data.OneOrMoreApp
import Data.Vector qualified as V
import Data.Bool
import Data.Word
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson qualified as A
import System.Timeout
import System.Random
import Nostr.Event qualified as Event
import Nostr.Event.Json qualified as EvJsn
import Nostr.Filter qualified as Filter
import Nostr.Filter.Json qualified as FlJsn
import Wuss qualified as Ws
import Network.WebSockets qualified as Ws

import Tools
import TryBech32

import Control.Moffy.Nostr.Event

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

printEvent' :: Event.E -> IO ()
printEvent' ev = do
	print ev
	T.putStrLn $ Event.content ev
	print $ lookupE ev

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

awaitEvent :: React s Events (T.Text, Event.E)
awaitEvent = adjust $ await EventReq \(OccEvent nm ev) -> (nm, ev)

awaitNameEventE :: T.Text -> React s Events (Event.E, Maybe Event.E)
awaitNameEventE nm0 = do
	ev <- awaitNameEvent nm0
	case lookupE ev of
		Nothing -> pure (ev, Nothing)
		Just e -> do
			let	nm = "bar" <> e
			request nm (filterId e)
			ev' <- awaitNameEvent nm
			pure (ev, Just ev')

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

{-
nameEventE :: T.Text -> Sig s Events [(Event.E, Maybe Event.E)] ()
nameEventE nm0 = do
	ev <- awaitNameEvent nm0
	case lookupE ev of
		Nothing -> do
			emit (ev, Nothing)
			nameEventE nm0
		Just e -> do
			nm <- randomName
			request nm (filterId e)
			-}

randomName :: IO T.Text
randomName = do
	n <- randomIO
	pure . T.pack $ "barbarbar" ++ show @Word64 n

randomName' :: React s EventsRnd T.Text
randomName' = do
	n <- adjust Rnd.getRandom
	pure . T.pack $ "barbarbar" ++ show @Word64 n

threadedName :: React s EventsThread T.Text
threadedName = do
	n <- adjust getThreadId
	pure . T.pack $ "barbarbar" ++ show n

awaitEose :: React s Events T.Text
awaitEose = adjust $ await EoseReq \(OccEose nm) -> nm

awaitNameEose :: T.Text -> React s Events ()
awaitNameEose nm0 = bool (awaitNameEose nm0) (pure ()) . (== nm0) =<< awaitEose

awaitNameEose' :: T.Text -> React s EventsRnd ()
awaitNameEose' nm0 = bool (awaitNameEose' nm0) (pure ()) . (== nm0) =<< adjust awaitEose

filterAuthor :: T.Text -> Either String Filter.Filter
filterAuthor pb = do
	pk <- Event.publicFromBech32 pb
	pure Filter.Filter {
		Filter.ids = Nothing,
		Filter.authors = Just [pk], Filter.kinds = Just [1],
		Filter.tags = [],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }

filterP :: T.Text -> Maybe Filter.Filter
filterP a = do
	pub <- toHex <$> dataPart' "npub" (chomp a)
	pure Filter.Filter {
		Filter.ids = Nothing,
		Filter.authors = Nothing, Filter.kinds = Just [1],
		Filter.tags = [('p', [pub])],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }

handleTChan :: TChan (EvReqs es) -> TChan (EvOccs es) -> Handle IO es
handleTChan cr co rq = do
	atomically $ writeTChan cr rq
	atomically $ readTChan co

handle' ::
	Rnd.RandomState st =>
	TChan (EvReqs Events) -> TChan (EvOccs Events) -> HandleSt st IO EventsRnd
handle' cr co = Handle.retrySt
	$ Handle.liftHandle' (TC.handle (Just 0.5) cr co) `Handle.mergeSt` Rnd.handleRandom

handle'' ::
	TChan (EvReqs Events) -> TChan (EvOccs Events) -> Handle IO EventsThread
handle'' cr co = Handle.retry
	$ TC.handle (Just 0.5) cr co `Handle.merge` handleGetThreadId

run :: (TVar Bool -> a -> IO ()) -> String -> String -> Sig s Events a r -> IO ()
run pr raddr rprt s = do
	ed <- atomically $ newTVar False
	cr <- atomically newTChan
	co <- atomically newTChan
	_ <- forkIO $ do
		_ <- interpret (handleTChan cr co) (pr ed) s
		pure ()
	Ws.runSecureClient raddr (read rprt) "/" $ ws "foo" cr co ed

run' :: (a -> IO ()) -> String -> String -> Sig s EventsRnd a r -> IO ()
run' pr raddr rprt s = do
	ed <- atomically $ newTVar False
	cr <- atomically newTChan
	co <- atomically newTChan
	_ <- forkIO $ do
		_ <- interpretSt (handle' cr co) pr s (mkStdGen 8)
		pure ()
	Ws.runSecureClient raddr (read rprt) "/" $ ws "foo" cr co ed

run'' :: (a -> IO ()) -> String -> String -> Sig s EventsThread a r -> IO ()
run'' pr raddr rprt s = do
	ed <- atomically $ newTVar False
	cr <- atomically newTChan
	co <- atomically newTChan
	_ <- forkIO $ do
		_ <- interpret (handle'' cr co) pr s
		pure ()
	Ws.runSecureClient raddr (read rprt) "/" $ ws "foo" cr co ed

ws :: T.Text ->
	TChan (EvReqs Events) -> TChan (EvOccs Events) ->
	TVar Bool -> Ws.ClientApp ()
ws _pub cr co ed cnn = do
	doWhile $ do
		putStrLn "before readTChan"
		e <- atomically $ readTVar ed
		putStrLn $ "ed = " ++ show e
		atomically (readTChan cr) >>= \r -> do
			putStrLn "readTChan done"
			a <- case OOM.project r of
				Nothing -> pure True
				Just (ReqReq nm fltr) -> do
					T.putStrLn nm
					let	r1 = req nm fltr
					Ws.sendTextData cnn r1
					atomically . writeTChan co . expand $ Singleton OccReq
					pure True
			b <- case OOM.project r of
				Nothing -> pure True
				Just HaltReq -> do
					putStrLn "HALT"
					atomically . writeTChan co . expand $ Singleton OccHalt
					pure False
			c <- case OOM.project r of
				Nothing -> pure True
				Just EventReq -> do
					putStrLn "BEFORE READ POST"
					_ <- timeout 1000000 $ readPost co cnn
					pure True
			_ <- case OOM.project r of
				Nothing -> pure True
				Just EndReq -> do
					putStrLn "END REQ"
					e' <- atomically $ readTVar ed
					putStrLn $ "END VAR: " ++ show e'
					atomically . when e' . writeTChan co . expand $ Singleton OccEnd
					pure True
			print $ a && b && c
--			pure $ a && b && c && (not e)
			pure $ a && b && c
	putStrLn "BYEBYE"
	Ws.sendClose cnn ("Bye!" :: T.Text)

readPost :: TChan (EvOccs Events) -> Ws.Connection -> IO ()
readPost co cnn = Ws.receiveData cnn >>= \rdt -> do
--	LBSC.putStrLn rdt
	case A.decode rdt of
		Just (A.Array (V.toList -> [
				A.String "EOSE",
				A.String nm ])) ->
			atomically . writeTChan co . expand . Singleton $ OccEose nm
		Just (A.Array (V.toList -> [
				A.String "EVENT",
				A.String nm,
				A.Object jsn ])) -> do
			Just ev <- pure $ EvJsn.decode jsn
			atomically . writeTChan co . expand
				. Singleton $ OccEvent nm ev
		Just _ -> pure (); Nothing -> error "bad"

req :: T.Text -> Filter.Filter -> LBSC.ByteString
req nm = A.encode . A.Array . V.fromList
	. ([A.String "REQ", A.String nm] ++) . (: []) . FlJsn.encode
