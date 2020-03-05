{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox.Handle (FollowboxIO, handle') where

import Foreign.C.Types
import Control.Monad.State
import Control.Concurrent
import Data.String
import Data.Time
import Data.Time.Clock.POSIX
import System.Exit
import System.Process
import Network.HTTP.Simple

import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

import React hiding (first)
import Event
import Field
import ButtonEvent

import Followbox.Event hiding (getCurrentTime, getCurrentTimeZone)
import BasicAuth

import qualified Graphics.X11.Xrender as Xr

type FollowboxIO = StateT ([Int], [Object], Maybe UTCTime) IO

convertXGlyphInfo :: Num n => Xr.XGlyphInfo -> XGlyphInfo n
convertXGlyphInfo (Xr.XGlyphInfo w_ h_ x_ y_ xo_ yo_) = XGlyphInfo w h x y xo yo
	where [w, h, x, y, xo, yo] = fromIntegral <$> [w_, h_, x_, y_, xo_, yo_]

first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (x, y, z) = (f x, y, z)

second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (x, y, z) = (x, f y, z)

third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (x, y, z) = (x, y, f z)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

getRandoms :: FollowboxIO [Int]
getRandoms = fst3 <$> get

putRandoms :: [Int] -> FollowboxIO ()
putRandoms rs = modify (const rs `first3`)

getObjects :: FollowboxIO [Object]
getObjects = snd3 <$> get

putObjects :: [Object] -> FollowboxIO ()
putObjects os = modify (const os `second3`)

getRateLimitReset :: FollowboxIO (Maybe UTCTime)
getRateLimitReset = trd3 <$> get

putRateLimitReset :: Maybe UTCTime -> FollowboxIO ()
putRateLimitReset rlr = modify (const rlr `third3`)

http :: Maybe (BS.ByteString, FilePath) -> String -> IO ([Header], LBS.ByteString)
http nmtkn u = do
	rsp <- case nmtkn of
		Just (nm, tkn) -> httpBasicAuth nm tkn
			. setRequestHeader "User-Agent" ["Yoshio"] $ fromString u
		Nothing -> httpLBS
			. setRequestHeader "User-Agent" ["Yoshio"] $ fromString u
	print $ getResponseHeader "X-RateLimit-Remaining" rsp
	print . (posixSecondsToUTCTime . fromInteger . read . BSC.unpack <$>) $ getResponseHeader "X-RateLimit-Reset" rsp
	pure (getResponseHeaders rsp, getResponseBody rsp)

isSleep :: FollowboxEvent n -> Bool
isSleep (Sleep _) = True
isSleep _ = False

handle' :: Field -> Maybe (BS.ByteString, FilePath) -> EvReqs (FollowboxEvent CInt) -> FollowboxIO (EvOccs (FollowboxEvent CInt))
handle' f nmtkn evs
	| Just (Sleep t) <- S.lookupMin $ S.filter isSleep evs = do
		now <- liftIO getCurrentTime
		if t > now
			then (liftIO . putStrLn $ "here: " ++ show evs) >> handle f nmtkn evs
			else pure . S.singleton $ Sleep t
	| otherwise = handle f nmtkn evs

handle :: Field -> Maybe (BS.ByteString, FilePath) -> EvReqs (FollowboxEvent CInt) -> FollowboxIO (EvOccs (FollowboxEvent CInt))
handle f nmtkn evs
	| Just (RaiseError (Cause em)) <- S.lookupMin $ S.filter (== RaiseError Response) evs = do
		liftIO do
			putStrLn $ "Error: " ++ em
--			threadDelay 30000000
		pure . S.singleton $ RaiseError Response
	| Just (Browse (Cause uri)) <- S.lookupMin $ S.filter (== Browse Response) evs =
		S.singleton (Browse Response) <$ liftIO (putStrLn uri >> rawSystem "firefox" [uri])
	| Just (LoadJsons Request) <- S.lookupMin $ S.filter (== LoadJsons Request) evs =
		S.singleton . LoadJsons . Occurred <$> getObjects
	| Just (HttpGet uri _) <- S.lookupMin $ S.filter isHttpGet evs =
		liftIO $ S.singleton . HttpGet uri . Occurred <$> http nmtkn uri
	| Just (StoreJsons (Cause os)) <- S.lookupMin $ S.filter (== StoreJsons Response) evs =
		S.singleton (StoreJsons Response) <$ putObjects os
	| Just (CalcTextExtents (Action (fn, fs, str))) <- S.lookupMin $ S.filter (== CalcTextExtents Communication) evs =
		(S.singleton . CalcTextExtents . Event <$> liftIO (convertXGlyphInfo <$> textExtents f fn fs str))
	| Just (StoreRandoms (Cause rs)) <- S.lookupMin $ S.filter (== StoreRandoms Response) evs =
		S.singleton (StoreRandoms Response) <$ putRandoms rs
	| Just (LoadRandoms Request) <- S.lookupMin $ S.filter (== LoadRandoms Request) evs =
		S.singleton . LoadRandoms . Occurred <$> getRandoms
	| Just (StoreRateLimitReset (Cause t)) <- S.lookupMin $ S.filter (== StoreRateLimitReset Response) evs =
		S.singleton (StoreRateLimitReset Response) <$ putRateLimitReset t
	| Just (LoadRateLimitReset Request) <- S.lookupMin $ S.filter (== LoadRateLimitReset Request) evs =
		S.singleton . LoadRateLimitReset  . Occurred <$> getRateLimitReset
	| Just (GetCurrentTime Request) <- S.lookupMin $ S.filter (== GetCurrentTime Request) evs =
		S.singleton . GetCurrentTime . Occurred <$> liftIO getCurrentTime
	| Just (GetCurrentTimeZone Request) <- S.lookupMin $ S.filter (== GetCurrentTimeZone Request) evs = do
		S.singleton . GetCurrentTimeZone . Occurred <$> liftIO getCurrentTimeZone
	| Just (SyncWaitMessage (Cause True)) <- S.lookupMin $ S.filter (== SyncWaitMessage (Cause True)) evs =
		pure . S.singleton $ SyncWaitMessage Response
	| LeftClick `S.member` evs = withNextEventTimeout' f 10000000
		$ \case	Just ev -> liftIO (putStrLn "foobar") >> handleEvent f nmtkn evs ev
			Nothing -> handle' f nmtkn evs
	| Just (Sleep t) <- S.lookupMin $ S.filter isSleep evs =
		S.singleton (Sleep t) <$ liftIO do
			putStrLn "hogepiyo"
			now <- getCurrentTime
			threadDelay . floor $ 1000000 * (t `diffUTCTime` now)
	| otherwise = pure S.empty

handleEvent :: Field -> Maybe (BS.ByteString, FilePath) -> EvReqs (FollowboxEvent CInt) -> Field.Event -> FollowboxIO (EvOccs (FollowboxEvent CInt))
handleEvent f nmtkn evs = \case
	DestroyWindowEvent {} -> liftIO $ closeField f >> exitSuccess
	ExposeEvent {} -> liftIO (flushField f) >> handle' f nmtkn evs
	ev -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = Button1,
			position = (x, y) } -> do
				(S.fromList [Followbox.Event.Move (Occurred (x, y)), LeftClick] <>)
					<$> handle' f nmtkn (S.filter (\r -> Followbox.Event.Move Request /= r && LeftClick /= r) evs)
		Just _ -> handle' f nmtkn evs
		Nothing	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle' f nmtkn evs
			| otherwise -> liftIO (print ev) >> handle' f nmtkn evs
