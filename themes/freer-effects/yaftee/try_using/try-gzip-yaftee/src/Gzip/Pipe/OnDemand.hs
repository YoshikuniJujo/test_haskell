{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gzip.Pipe.OnDemand where

data Request
	= RequestBits Int
	| RequestBytes Int
	| RequestBuffer Int
	| RequestString
--	| RequestPushBack BitArray.B
	deriving Show
