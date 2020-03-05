{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox.DummyHandle (FollowboxIO, handle') where

import Foreign.C.Types
import Control.Monad.State
import Control.Concurrent
import Data.Time
import Data.Time.Clock.POSIX
import Data.IORef
import System.IO.Unsafe
import System.Exit
import System.Process
import Network.HTTP.Simple

import System.Environment

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

import React hiding (first)
import Event
import Field
import ButtonEvent

import Followbox
import Followbox.AesonObject
import Followbox.DummyUsers

import qualified Graphics.X11.Xrender as Xr

remain :: IORef Int
remain = unsafePerformIO $ newIORef =<< (<$> getArgs) \case
	["0"] -> 0; _ -> 10

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

isAvatarUrl :: String -> Bool
isAvatarUrl url
	| "https://avatars" `L.isPrefixOf` url = True
	| otherwise = False

http :: String -> IO ([Header], LBS.ByteString)
http u | isAvatarUrl u = ([] ,) <$> LBS.readFile "lenna.png"
http u = do
	r <- readIORef remain
	case r of
		_ | r > 0 -> do
			modifyIORef remain (subtract 1)
			pure (	[("X-RateLimit-Remaining", BSC.pack $ show r), ("X-RateLimit-Reset", "1583200300")],
				dummyUsers . read $ drop 35 u)
		_ -> do	writeIORef remain 10
			now <- getCurrentTime
			pure (	[	("X-RateLimit-Remaining", "0"),
					("X-RateLImit-Reset", BSC.pack . show @Int . floor $ utcTimeToPOSIXSeconds now + 30) ], "" )

isSleep :: FollowboxEvent n -> Bool
isSleep (Sleep _) = True
isSleep _ = False

handle' :: Field -> EvReqs (FollowboxEvent CInt) -> FollowboxIO (EvOccs (FollowboxEvent CInt))
handle' f evs
	| Just (Sleep t) <- S.lookupMin $ S.filter isSleep evs = do
		liftIO $ putStrLn $ "handle' (Sleep): " ++ show (S.filter (/= StoreRandoms Response) evs)
		getRateLimitReset >>= liftIO . print
		now <- liftIO getCurrentTime
		if t > now
			then (liftIO (putStrLn $ "here: " ++ show evs) >> handle f evs)
			else pure . S.singleton $ Sleep t
	| otherwise = do
		liftIO $ putStrLn $ "handle' (no Sleep): " ++ show (S.filter (/= StoreRandoms Response) evs)
		handle f evs

handle :: Field -> EvReqs (FollowboxEvent CInt) -> FollowboxIO (EvOccs (FollowboxEvent CInt))
handle f evs
	| Just (Error (Cause em)) <- S.lookupMin $ S.filter (== Error Response) evs = do
		liftIO do
			putStrLn $ "Error: " ++ em
			threadDelay 30000000
		pure . S.singleton $ Error Response
	| Just (Browse (Cause uri)) <- S.lookupMin $ S.filter (== Browse Response) evs =
		S.singleton (Browse Response) <$ liftIO (putStrLn uri >> rawSystem "firefox" [uri])
	| Just (LoadJsons Request) <- S.lookupMin $ S.filter (== LoadJsons Request) evs =
		S.singleton . LoadJsons . Occurred <$> getObjects
	| Just (Http uri _) <- S.lookupMin $ S.filter isHttp evs =
		liftIO $ S.singleton . Http uri . Occurred <$> http uri
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
	| Just (WaitMessage (Cause True)) <- S.lookupMin $ S.filter (== WaitMessage (Cause True)) evs =
		pure . S.singleton $ WaitMessage Response
	| LeftClick `S.member` evs = withNextEventTimeout' f 10000000
		$ \case	Just ev -> liftIO (putStrLn "foobar") >> handleEvent f evs ev
			Nothing -> handle' f evs
	| Just (Sleep t) <- S.lookupMin $ S.filter isSleep evs =
		S.singleton (Sleep t) <$ liftIO do
			putStrLn "hogepiyo"
			now <- getCurrentTime
			threadDelay . floor $ 1000000 * (t `diffUTCTime` now)
	| otherwise = pure S.empty

isHttp :: FollowboxEvent n -> Bool
isHttp (Http _ _) = True
isHttp _ = False

handleEvent :: Field -> EvReqs (FollowboxEvent CInt) -> Field.Event -> FollowboxIO (EvOccs (FollowboxEvent CInt))
handleEvent f evs = \case
	DestroyWindowEvent {} -> liftIO $ closeField f >> exitSuccess
	ExposeEvent {} -> liftIO (flushField f) >> handle' f evs
	ev -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = Button1,
			position = (x, y) } -> do
				(S.fromList [Followbox.Move (Occurred (x, y)), LeftClick] <>)
					<$> handle' f (S.filter (\r -> Followbox.Move Request /= r && LeftClick /= r) evs)
		Just _ -> handle' f evs
		Nothing	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle' f evs
			| otherwise -> liftIO (print ev) >> handle' f evs
