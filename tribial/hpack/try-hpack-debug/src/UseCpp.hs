{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseCpp where

foo :: Int
foo =
#ifdef FOO
	8
#else
	123
#endif

