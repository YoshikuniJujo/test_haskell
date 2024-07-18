{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sample (foo) where

import Hason

foo :: Hason
foo = [
	(KStr "foo", Str "bar"),
	(KStr "hoge", Int 123),
	(KStr "seq", Seq [Str "foo", Int 345]),
	(KStr "dct", Dct [
		(KInt 456, Str "four-five-six"),
		(KStr "boo", Int 567)])
	]
