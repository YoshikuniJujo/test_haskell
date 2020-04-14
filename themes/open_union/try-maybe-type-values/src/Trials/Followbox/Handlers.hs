{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Handlers where

import Prelude hiding ((++))

import Control.Monad.State
import Data.Type.Set
import Data.String
import Data.UnionSet hiding (merge)

import qualified Data.ByteString as BS
import qualified Network.HTTP.Simple as HTTP

import Trials.Followbox.Event
import Trials.Followbox.BasicAuth
import Trials.Followbox.Aeson
import Trials.Followbox.XGlyphInfo
import MonadicFrp.Handle
import Field hiding (textExtents)

handleHttpGet :: Maybe (BS.ByteString, FilePath) -> EvReqs (Singleton HttpGet) -> IO (EvOccs (Singleton HttpGet))
handleHttpGet mba reqs = do
	r <- hg . setUserAgent "Yoshio" $ fromString u
	print $ HTTP.getResponseHeader "X-RateLimit-Remaining" r
	pure . singleton $ OccHttpGet u (HTTP.getResponseHeaders r) (HTTP.getResponseBody r)
	where
	hg = case mba of
		Just (nm, tkn) -> httpBasicAuth nm tkn
		Nothing -> HTTP.httpLBS
	HttpGetReq u = extract reqs

setUserAgent :: BS.ByteString -> HTTP.Request -> HTTP.Request
setUserAgent ua = HTTP.setRequestHeader "User-Agent" [ua]

handleStoreJsons :: Monad m => EvReqs (Singleton StoreJsons) -> StateT [Object] m (EvOccs (Singleton StoreJsons))
handleStoreJsons reqs = singleton (OccStoreJsons os) <$ put os
	where StoreJsons os = extract reqs

handleLoadJsons :: Monad m => EvReqs (Singleton LoadJsons) -> StateT [Object] m (EvOccs (Singleton LoadJsons))
handleLoadJsons _reqs = singleton . OccLoadJsons <$> get

handleLeftClick :: EvReqs (LeftClick :- Quit :- 'Nil) -> IO (EvOccs (LeftClick :- Quit :- 'Nil))
handleLeftClick reqs = getLine >>= \case
	"" -> liftIO (putStrLn "here") >> pure (expand $ singleton OccLeftClick)
	"q" -> pure (expand $ singleton OccQuit)
	_ -> handleLeftClick reqs

handleRaiseError :: EvReqs (Singleton RaiseError) -> IO (Maybe (EvOccs (Singleton RaiseError)))
handleRaiseError reqs = case e of
	NotJson -> do
		putStrLn $ "ERROR: " <> em
		pure . Just . singleton $ OccRaiseError e Terminate
	NoLoginName -> do
		putStrLn $ "ERROR: " <> em
		pure . Just . singleton $ OccRaiseError e Terminate
	CatchError -> pure Nothing
	where RaiseError e em = extract reqs

dummyHandleCalcTextExtents :: EvReqs (Singleton CalcTextExtents) -> IO (Maybe (EvOccs (Singleton CalcTextExtents)))
dummyHandleCalcTextExtents _reqs = pure Nothing

handle :: Maybe (BS.ByteString, FilePath) -> Handle (StateT [Object] IO) FollowboxEv
handle mba = retry $
	(Just <$>) . handleStoreJsons `merge`
	liftIO . (Just <$>) . handleHttpGet mba `merge`
	(Just <$>) . handleLoadJsons `merge`
	liftIO . dummyHandleCalcTextExtents `merge`
	liftIO . handleRaiseError `before`
	liftIO . (Just <$>) . handleLeftClick

handle' :: Field -> Maybe (BS.ByteString, FilePath) -> Handle (StateT [Object] IO) FollowboxEv
handle' f mba = retry $
	(Just <$>) . handleStoreJsons `merge`
	liftIO . (Just <$>) . handleHttpGet mba `merge`
	(Just <$>) . handleLoadJsons `merge`
	liftIO . (Just <$>) . handleCalcTextExtents f `merge`
	liftIO . handleRaiseError `before`
	liftIO . handleLeftClick' f

handleLeftClick' :: Field -> EvReqs (LeftClick :- Quit :- 'Nil) -> IO (Maybe (EvOccs (LeftClick :- Quit :- 'Nil)))
handleLeftClick' f _reqs = withNextEvent f \case
	ButtonEvent { ev_event_type = 4, ev_button = 1 } -> pure . Just . expand . singleton $ OccLeftClick
	ButtonEvent { ev_event_type = 4, ev_button = 3 } -> pure . Just . expand . singleton $ OccQuit
	_ -> pure Nothing

handleCalcTextExtents :: Field -> EvReqs (Singleton CalcTextExtents) -> IO (EvOccs (Singleton CalcTextExtents))
handleCalcTextExtents f reqs = singleton . OccCalcTextExtents fn fs t <$> textExtents f fn fs t
	where CalcTextExtentsReq fn fs t = extract reqs
