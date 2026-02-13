{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Event.Signed (

	E(..), signature, verify, verify'

	) where

import Data.Bool
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

import Nostr.Event qualified as Event

data E = E {
	idnt :: BS.ByteString,
	pubkey :: Pub,
	created_at :: UnixTime,
	kind :: Int,
	tags :: [(T.Text, (T.Text, [T.Text]))],
	content :: T.Text,
	sig :: BS.ByteString,
	verified :: Bool }
	deriving Show

signature :: Event.Secret -> Event.E -> IO E
signature sec e = do
	Just sg <- Event.signature sec e
	pure E {
		idnt = Event.hash e,
		pubkey = Event.pubkey e,
		created_at = Event.created_at e,
		kind = Event.kind e,
		tags = Event.tags e,
		content = Event.content e,
		sig = sg,
		verified = True }

verify :: E -> Maybe Event.E
verify e = let e' = Event.E {
	Event.pubkey = pubkey e,
	Event.created_at = created_at e,
	Event.kind = kind e,
	Event.tags = tags e,
	Event.content = content e } in
	bool Nothing (Just e') $
		verified e ||
		idnt e == Event.hash e' && Event.verify e' (sig e)

verify' :: E -> Maybe E
verify' e@E { verified = True } = Just e
verify' e = e { verified = True } <$ verify e
