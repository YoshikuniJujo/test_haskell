{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.Sig.Type (
	-- * Type
	Sig(..), ISig(..), isig,
	-- * Function
	-- ** Basic
	emit, emitAll, waitFor, res, ires, hold,
	-- ** Practical
	repeat, scanl, find ) where

import Prelude hiding (repeat, scanl)

import Control.Monad
import Data.Type.Flip

import Control.Moffy.Internal.React.Type

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

repeat :: React s es a -> Sig s es a ()
repeat = forever . (emit <=< waitFor)

scanl :: (b -> a -> b) -> b -> Sig s es a r -> Sig s es b r
scanl op v = emitAll . iscanl op v

iscanl :: (b -> a -> b) -> b -> Sig s es a r -> ISig s es b r
iscanl op v (Sig r) = v :| (isig pure (scanl op . (v `op`)) =<< waitFor r)

find :: (a -> Bool) -> Sig s es a r -> React s es (Either a r)
find p = (icur <$>) . res . brk
	where
	brk = Sig . (ibrk <$>) . unSig
	ibrk = \case
		is@(End _) -> pure is
		is@(h :| t) | p h -> pure is | otherwise -> h :| brk t

icur :: ISig s es a b -> Either a b
icur = isig Right (const . Left)

res :: Sig s es a b -> React s es b
res = ires <=< unSig

ires :: ISig s es a b -> React s es b
ires = isig pure (const res)
