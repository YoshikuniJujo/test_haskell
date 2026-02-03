{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Signed where

import Prelude hiding (id)
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

import Event qualified as Event

data E = E {
	id :: BS.ByteString,
	pubkey :: Pub,
	created_at :: UnixTime,
	kind :: Int,
	tags :: Map.Map T.Text (T.Text, [T.Text]),
	content :: T.Text,
	sig :: BS.ByteString }
	deriving Show

signature :: Event.Secret -> Event.E -> IO E
signature sec e = do
	Just sg <- Event.signature sec e
	pure E {
		id = Event.hash e,
		pubkey = Event.pubkey e,
		created_at = Event.created_at e,
		kind = Event.kind e,
		tags = Event.tags e,
		content = Event.content e,
		sig = sg }

verify :: E -> Maybe Event.E
verify e = let e' = Event.E {
	Event.pubkey = pubkey e,
	Event.created_at = created_at e,
	Event.kind = kind e,
	Event.tags = tags e,
	Event.content = content e
	} in
	if id e == Event.hash e' && Event.verify e' (sig e)
	then Just e'
	else Nothing
