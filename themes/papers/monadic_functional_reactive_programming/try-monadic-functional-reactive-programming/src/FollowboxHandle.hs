{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FollowboxHandle (FollowboxIO, handle) where

import Foreign.C.Types
import Control.Arrow
import Control.Monad.State
import Control.Concurrent
import Data.String
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

import Followbox
import AesonObject
import BasicAuth

import qualified Graphics.X11.Xrender as Xr

type FollowboxIO = StateT ([Int], [Object]) IO

convertXGlyphInfo :: Num n => Xr.XGlyphInfo -> XGlyphInfo n
convertXGlyphInfo (Xr.XGlyphInfo w_ h_ x_ y_ xo_ yo_) = XGlyphInfo w h x y xo yo
	where [w, h, x, y, xo, yo] = fromIntegral <$> [w_, h_, x_, y_, xo_, yo_]

getRandoms :: FollowboxIO [Int]
getRandoms = fst <$> get

putRandoms :: [Int] -> FollowboxIO ()
putRandoms rs = modify (const rs `first`)

getObjects :: FollowboxIO [Object]
getObjects = snd <$> get

putObjects :: [Object] -> FollowboxIO ()
putObjects os = modify (const os `second`)

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

handle :: Field -> Maybe (BS.ByteString, FilePath) -> EvReqs (FollowboxEvent CInt) -> FollowboxIO (EvOccs (FollowboxEvent CInt))
handle f nmtkn evs
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
		liftIO $ S.singleton . Http uri . Occurred <$> http nmtkn uri
	| Just (StoreJsons (Cause os)) <- S.lookupMin $ S.filter (== StoreJsons Response) evs =
		S.singleton (StoreJsons Response) <$ putObjects os
	| Just (CalcTextExtents (Action (fn, fs, str))) <- S.lookupMin $ S.filter (== CalcTextExtents Communication) evs =
		(S.singleton . CalcTextExtents . Event <$> liftIO (convertXGlyphInfo <$> textExtents f fn fs str))
	| Just (StoreRandoms (Cause rs)) <- S.lookupMin $ S.filter (== StoreRandoms Response) evs =
		S.singleton (StoreRandoms Response) <$ putRandoms rs
	| Just (LoadRandoms Request) <- S.lookupMin $ S.filter (== LoadRandoms Request) evs =
		S.singleton . LoadRandoms . Occurred <$> getRandoms
	| LeftClick `S.member` evs = withNextEvent f (handleEvent f nmtkn evs)
	| otherwise = pure S.empty

isHttp :: FollowboxEvent n -> Bool
isHttp (Http _ _) = True
isHttp _ = False

handleEvent :: Field -> Maybe (BS.ByteString, FilePath) -> EvReqs (FollowboxEvent CInt) -> Field.Event -> FollowboxIO (EvOccs (FollowboxEvent CInt))
handleEvent f nmtkn evs = \case
	DestroyWindowEvent {} -> liftIO $ closeField f >> exitSuccess
	ExposeEvent {} -> liftIO (flushField f) >> handle f nmtkn evs
	ev -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = Button1,
			position = (x, y) } -> do
				(S.fromList [Followbox.Move (Occurred (x, y)), LeftClick] <>)
					<$> handle f nmtkn (S.filter (\r -> Followbox.Move Request /= r && LeftClick /= r) evs)
		Just _ -> handle f nmtkn evs
		Nothing	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle f nmtkn evs
			| otherwise -> liftIO (print ev) >> handle f nmtkn evs
