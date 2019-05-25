{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

data Foo = Foo Integer String deriving Show

instance Num Foo where
	negate f@(Foo n s) = Foo (negate n) $ "negate (" ++ show f ++ ")"
	fromInteger n = Foo n $ "fromInteger " ++ show n

data Bar = Bar Word String deriving Show

instance Num Bar where
	negate f@(Bar n s) = Bar (negate n) $ "negate (" ++ show f ++ ")"
	fromInteger n = Bar (fromInteger n) $ "fromInteger " ++ show n
