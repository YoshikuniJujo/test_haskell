{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Event.NoPub (E(..), addPubKey) where

import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

import Nostr.Event qualified as Event

data E = E {
	created_at :: UnixTime, kind :: Int,
	tags :: [(T.Text, (T.Text, [T.Text]))], content :: T.Text }
	deriving Show

addPubKey :: E -> Pub -> Event.E
addPubKey e p = Event.E {
	Event.pubkey = p,
	Event.created_at = created_at e,
	Event.kind = kind e,
	Event.tags = tags e,
	Event.content = content e }
