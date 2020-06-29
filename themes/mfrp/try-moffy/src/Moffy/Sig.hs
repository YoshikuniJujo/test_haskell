{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.Sig where

import Prelude hiding (scanl)

import Control.Monad
import Data.Type.Flip

import Moffy.React

infixr 5 :|
newtype Sig s es a r = Sig { unSig :: React s es (ISig s es a r) }
data ISig s es a r = End r | a :| Sig s es a r

isig :: (r -> b) -> (a -> Sig s es a r -> b) -> ISig s es a r -> b
isig e c = \case End x -> e x; h :| t -> c h t

instance Functor (Sig s es a) where f `fmap` Sig s = Sig $ (f <$>) <$> s

instance Applicative (Sig s es a) where
	pure = emitAll . pure
	Sig rf <*> mx = Sig $ isig
		(unSig . (<$> mx)) (\h -> pure . (h :|) . ((<$> mx) =<<)) =<< rf

instance Monad (Sig s es a) where
	Sig r >>= f =
		Sig $ isig (unSig . f) (\h -> pure . (h :|) . (f =<<)) =<< r

instance Functor (ISig s es a) where
	fmap f = isig (End . f) (\h -> (h :|) . (f <$>))

instance Applicative (ISig s es a) where
	pure = End
	mf <*> mx = isig (<$> mx) (\h -> (h :|) . (emitAll . (<$> mx) =<<)) mf

instance Monad (ISig s es a) where
	m >>= f = isig f (\h -> (h :|) . (emitAll . f =<<)) m

instance Functor (Flip (Sig s es) r) where
	fmap f = Flip . Sig . ((f <$%>) <$>) . unSig . unflip

instance Functor (Flip (ISig s es) r) where
	fmap f = Flip . isig pure (\h -> (f h :|) . (f <$%>)) . unflip

emit :: a -> Sig s es a ()
emit = emitAll . (:| pure ())

emitAll :: ISig s es a b -> Sig s es a b
emitAll = Sig . pure

hold :: Sig s es a r
hold = waitFor never

waitFor :: React s es r -> Sig s es a r
waitFor = Sig . (pure <$>)

interpret :: Monad m => Handle m es -> (a -> m ()) -> Sig s es a r -> m r
interpret hdl vw = go where
	go (Sig s) = interpretReact hdl s >>= isig pure \h -> (vw h >>) . go

interpretSt ::
	Monad m => st -> HandleSt st m es -> (a -> m ()) -> Sig s es a r -> m r
interpretSt st0 hdl vw = go st0 where
	go st (Sig s) = interpretReactSt st hdl s >>= \(is, st') ->
		isig pure (\h -> (vw h >>) . go st') is

repeat :: React s es a -> Sig s es a ()
repeat = forever . (emit <=< waitFor)

scanl :: (b -> a -> b) -> b -> Sig s es a r -> Sig s es b r
scanl op v = emitAll . iscanl op v

iscanl :: (b -> a -> b) -> b -> Sig s es a r -> ISig s es b r
iscanl op v (Sig r) = v :| (isig pure (scanl op . (v `op`)) =<< waitFor r)
