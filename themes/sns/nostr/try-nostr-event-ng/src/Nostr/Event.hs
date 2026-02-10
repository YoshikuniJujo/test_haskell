{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Event where

import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

data E = E {
	pubkey :: Pub,
	created_at :: UnixTime,
	kind :: Int,
	tags :: [(T.Text, (T.Text, [T.Text]))],
	content :: T.Text }
	deriving Show
