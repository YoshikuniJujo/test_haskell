{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseCpp where

foo :: String
foo =
#ifdef FOO
	"foo is true"
#else
	"foo is false"
#endif

bar :: String
bar =
#ifdef BAR
	"bar is true"
#else
	"bar is false"
#endif
