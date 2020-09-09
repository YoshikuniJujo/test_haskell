{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.Sig.Type (
	-- * Type
	Sig(..), ISig(..), isig,
	-- * Function
	-- ** Basic
	emit, emitAll, waitFor, res, ires, hold,
	-- ** Practical
	repeat, find, scanl ) where

import Prelude hiding (repeat, scanl)

import Control.Monad (forever, (<=<))
import Control.Moffy.Internal.React.Type (React, never)
import Data.Type.Flip (Flip(..), (<$%>))
import Data.Bool (bool)

---------------------------------------------------------------------------

-- * TYPE
-- * CLASS INSTANCE
--	+ MONAD
--	+ FLIP FUNCTOR
-- * FUNCTION
--	+ BASIC
--	+ PRACTICAL

---------------------------------------------------------------------------
-- TYPE
---------------------------------------------------------------------------

infixr 5 :|
newtype Sig s es a r = Sig { unSig :: React s es (ISig s es a r) }
data ISig s es a r = End r | a :| Sig s es a r

isig :: (r -> b) -> (a -> Sig s es a r -> b) -> ISig s es a r -> b
isig e c = \case End x -> e x; h :| t -> c h t

---------------------------------------------------------------------------
-- CLASS INSTANCE
---------------------------------------------------------------------------

-- MONAD

instance Functor (Sig s es a) where fmap f = Sig . ((f <$>) <$>) . unSig

instance Applicative (Sig s es a) where
	pure = emitAll . pure; Sig rf <*> (flip (<$>) -> ax) =
		Sig $ isig (unSig . ax) (\h -> pure . (h :|) . (ax =<<)) =<< rf

instance Monad (Sig s es a) where
	Sig r >>= f =
		Sig $ isig (unSig . f) (\h -> pure . (h :|) . (f =<<)) =<< r

instance Functor (ISig s es a) where
	fmap f = isig (End . f) \h -> (h :|) . (f <$>)

instance Applicative (ISig s es a) where
	pure = End; mf <*> (flip (<$>) -> ax) =
		isig ax (\h -> (h :|) . (emitAll . ax =<<)) mf

instance Monad (ISig s es a) where
	m >>= f = isig f (\h -> (h :|) . (emitAll . f =<<)) m

-- FLIP FUNCTOR

instance Functor (Flip (Sig s es) r) where
	fmap f = Flip . Sig . ((f <$%>) <$>) . unSig . unflip

instance Functor (Flip (ISig s es) r) where
	fmap f = Flip . isig pure (\h -> (f h :|) . (f <$%>)) . unflip

---------------------------------------------------------------------------
-- FUNCTION
---------------------------------------------------------------------------

-- BASIC

emit :: a -> Sig s es a ()
emit = emitAll . (:| pure ())

emitAll :: ISig s es a r -> Sig s es a r
emitAll = Sig . pure

waitFor :: React s es r -> Sig s es a r
waitFor = Sig . (pure <$>)

res :: Sig s es a r -> React s es r
res = ires <=< unSig

ires :: ISig s es a r -> React s es r
ires = isig pure $ const res

hold :: Sig s es a r
hold = waitFor never

-- PRACTICAL

repeat :: React s es a -> Sig s es a r
repeat = forever . (emit <=< waitFor)

find :: (a -> Bool) -> Sig s es a r -> React s es (Either a r)
find p = go where
	go = igo <=< unSig
	igo = isig (pure . Right) \h -> bool go (const . pure $ Left h) (p h)

scanl :: (b -> a -> b) -> b -> Sig s es a r -> Sig s es b r
scanl = ((emitAll .) .) . iscanl

iscanl :: (b -> a -> b) -> b -> Sig s es a r -> ISig s es b r
iscanl op v (Sig r) = v :| (isig pure (scanl op . (v `op`)) =<< waitFor r)
