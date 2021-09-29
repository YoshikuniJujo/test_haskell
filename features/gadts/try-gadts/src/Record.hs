{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Record where

data Foo a where
	Foo :: { fooInt :: Int, fooDouble :: Float, fooChar :: Char } -> Foo Int

deriving instance Show (Foo a)
