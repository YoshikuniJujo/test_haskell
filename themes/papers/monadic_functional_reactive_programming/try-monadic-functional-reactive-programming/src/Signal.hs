{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Signal where

import React

newtype Sig s e a b = Sig { unSig :: React s e (ISig s e a b) }
data ISig s e a b = End b | a :| Sig s e a b

interpretSig :: Monad m =>
	(EvReqs e -> m (EvOccs e)) -> (a -> m r) -> Sig s e a b -> m b
interpretSig p d (Sig s) = interpret p s >>=
	\case h :| t -> d h >> interpretSig p d t; End a -> pure a

waitFor :: React s e b -> Sig s e a b
waitFor x = Sig $ End <$> x

repeat :: React s e a -> Sig s e a ()
repeat x = xs where xs = Sig $ (:| xs) <$> x

instance Functor (Sig s e a) where
	f `fmap` Sig l = Sig $ (f <$>) <$> l

instance Applicative (Sig s e a) where
	pure = Sig . pure . pure
	Sig l <*> mx = Sig $ l >>= \case
		End f -> unSig $ f <$> mx
		h :| t -> pure $ h :| ((<$> mx) =<< t)

instance Monad (Sig s e a) where
	Sig l >>= f = Sig $ l >>=
		\case End x -> unSig $ f x; h :| t -> pure $ h :| (f =<< t)

emit :: a -> Sig s e a ()
emit x = emitAll $ x :| pure ()

emitAll :: ISig s e a b -> Sig s e a b
emitAll = Sig . pure

instance Functor (ISig s e a) where
	f `fmap` End x = End $ f x
	f `fmap` (h :| t) = h :| (f <$> t)

instance Applicative (ISig s e a) where
	pure = End
	End f <*> mx = f <$> mx
	(h :| t) <*> mx = h :| (t >>= emitAll . (<$> mx))

instance Monad (ISig s e a) where
	End x >>= f = f x
	(h :| t) >>= f = h :| (t >>= emitAll . f)
