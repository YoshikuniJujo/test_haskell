{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckSharing.EvInt where

import Control.Moffy
import Data.Type.Set
import Data.OneOrMore
import Control.Monad.Freer.Par

import Control.Moffy.Internal.React
import Control.Moffy.Internal.React.Type

data EvInt = EvIntReq deriving (Show, Eq, Ord)
numbered 9 [t| EvInt |]
instance Request EvInt where
	newtype Occurred EvInt = OccEvInt Int deriving Show

evInt :: React s (Singleton EvInt) Int
evInt = await EvIntReq \(OccEvInt n) -> n

evInt2 :: React s (Singleton EvInt) (Int, Int)
evInt2 = (,) <$> evInt <*> evInt

fromPure :: React s es a -> a
fromPure (Pure x) = x
fromPure _ = error "not pure"

singleOccEvInt :: Int -> EvOccs (Singleton EvInt)
singleOccEvInt = Singleton . OccEvInt

update' :: React s es a -> React s es a -> EvOccs es -> (React s es a, React s es a)
update' l r  = update l rootThreadId r rootThreadId

mkWrongPair :: Unique s (React s (Singleton EvInt) (Int, Int), React s (Singleton EvInt) (Int, Int))
mkWrongPair = do
	ei <- tag evInt2
	let	(ei1, _ei2) = update' ei ei $ singleOccEvInt 123
		(_ei3, ei4) = update' ei ei $ singleOccEvInt 321
	pure (ei1, ei4)

mkWrongResult :: Unique s ((Int, Int), (Int, Int))
mkWrongResult = do
	(l, r) <- mkWrongPair
	let	(l', r') = update' l r $ singleOccEvInt 444
	pure (fromPure l', fromPure r')
