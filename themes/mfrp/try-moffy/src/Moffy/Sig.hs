{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.Sig where

import Prelude hiding (scanl)

import Data.Type.Set

import qualified Control.Arrow as Arr

import Moffy.React
import Moffy.React.Common
import Moffy.Sig.Common
import Freer

pause :: Parable es (ISig s es a r) es' r' =>
	Sig s es a r -> React s es' r' -> Sig s (es :+: es') a (Sig s es a r, React s es' r')
Sig l `pause` r = waitFor (l `par` r) >>= \case
	(Pure l', r') -> (emitAll `Arr.first`) <$> emitAll (l' `ipause` r')
	(l', r'@(Pure _)) -> pure (Sig l', r')
	(Await _ :>>= _, Await _ :>>= _) -> error "never occur"
	(Never :>>= _, Await _ :>>= _) -> error "never occur"
	(_, Never :>>= _) -> error "never occur"
	(Never :>>= _, _) -> error "never occur"
	(GetThreadId :>>= _, _) -> error "never occur"
	(_, GetThreadId :>>= _) -> error "never occur"

ipause :: Parable es (ISig s es a r) es' r' =>
	ISig s es a r -> React s es' r' -> ISig s (es :+: es') a (ISig s es a r, React s es' r')
l@(End _) `ipause` r = pure (l, r)
(h :| t) `ipause` r = (h :|) $ (<$> (t `pause` r)) \case
	(Sig (Pure t'), r') -> (t', r')
	(t', r'@(Pure _)) -> (h :| t', r')
	_ -> error "never occur"

infixr 7 `at`

at :: (Parable es (ISig s es a r) es' r', Adjustable es (es :+: es')) =>
	Sig s es a r -> React s es' r' -> React s (es :+: es') (Either r (a, r'))
l `at` r = res (l `pause` r) >>= \(Sig l', r') -> (<$> adjust l') \case
	End x -> Left x
	h :| _ -> case r' of Pure y -> Right (h, y); _ -> error "never occur"

infixl 7 `until`

until :: (Parable es (ISig s es a r) es' r', Adjustable es (es :+: es')) =>
	Sig s es a r -> React s es' r' -> Sig s (es :+: es') a (Either r (a, r'))
l `until` r = l `pause` r >>= \(Sig l', r') -> (<$> waitFor (adjust l')) \case
	End x -> Left x
	h :| _ -> case r' of Pure y -> Right (h, y); _ -> error "never occur"
