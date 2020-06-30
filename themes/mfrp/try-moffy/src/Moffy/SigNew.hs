{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.SigNew where

import Data.Type.Set
import Data.OneOrMore

import qualified Control.Arrow as Arr

import Moffy.ReactNew
import Moffy.React.Common
import Moffy.Sig.Common
import Freer

Sig l `pause` r = waitFor (l `par` r) >>= \case
	(Pure l', r') -> (emitAll `Arr.first`) <$> emitAll (l' `ipause` r')
	(l', r'@(Pure _)) -> pure (Sig l', r')
	_ -> error "never occur"

l@(End _) `ipause` r = pure (l, r)
(h :| t) `ipause` r = (h :|) $ (<$> (t `pause` r)) \case
	(Sig (Pure t'), r') -> (t', r')
	(t', r'@(Pure _)) -> (h :| t', r')
	_ -> error "never occur"

infixr 7 `at`

at :: (
	Update (ISig s (es :+: es') a r) r',
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	Expandable es (es :+: es'), Expandable es' (es :+: es'),
	Mergeable (es :+: es') (es :+: es') (es :+: es')
	) => Sig s es a r -> React s es' r' -> React s (es :+: es') (Either r (a, r'))
l `at` r = res (adjustSig l `pause` adjust r) >>= \(Sig l', r') -> (<$> l') \case
	End x -> Left x
	h :| _ -> case r' of Pure y -> Right (h, y); _ -> error "never occur"

adjustSig :: (
	CollapsableOccurred es' es,
	Expandable es es'
	) => Sig s es a r -> Sig s es' a r
adjustSig (Sig r) = Sig $ adjustISig <$> adjust r 

adjustISig :: (
	CollapsableOccurred es' es,
	Expandable es es'
	) => ISig s es a r -> ISig s es' a r
adjustISig (End x) = End x
adjustISig (h :| t) = h :| adjustSig t
