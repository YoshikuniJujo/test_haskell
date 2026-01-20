{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event where

import Data.Map qualified as Map
import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

data Event = Event {
	pubkey :: Pub,
	created_at :: UnixTime,
	kind :: Int,
	tags :: Map.Map T.Text (T.Text, [T.Text]),
	content :: T.Text }
	deriving Show
