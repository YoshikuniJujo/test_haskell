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
import MonadicFrp.Handle

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

handle :: Maybe (BS.ByteString, FilePath) -> Handle (StateT [Object] IO) FollowboxEv
handle mba = retry $
	(Just <$>) . handleStoreJsons `merge`
	liftIO . (Just <$>) . handleHttpGet mba `merge`
	(Just <$>) . handleLoadJsons `merge`
	liftIO . handleRaiseError `before`
	liftIO . (Just <$>) . handleLeftClick
