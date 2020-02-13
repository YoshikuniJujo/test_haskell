{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TaggedIteratee where

import FTCQueue
import Iteratee hiding (Done, Get, val, (>>=^), get)
import qualified Iteratee as I

newtype Count s a = Count { unCount :: Integer -> (a, Integer) }

instance Functor (Count s) where
	f `fmap` m = pure . f =<< m

instance Applicative (Count s) where
	pure x = Count \c -> (x, c)
	mf <*> mx = (<$> mx) =<< mf

instance Monad (Count s) where
	Count k >>= f = Count \c -> let (x, c') = k c in unCount (f x) c'

countup :: Count s Integer
countup = Count \c -> (c, c + 1)

addTagsExp :: MExp m a b -> Count s (TaggedExp s m a b)
addTagsExp me = do
	tg <- countup
	case tviewl me of
		TOne (MCont f) -> pure . tsingleton $ Tagged tg f
		MCont f :| t -> ((tsingleton $ Tagged tg f) ><) <$> addTagsExp t

remTagsExp :: TaggedExp s m a b -> MExp m a b
remTagsExp te = case tviewl te of
	TOne (Tagged _ f) -> tsingleton $ MCont f
	Tagged _ f :| t -> tsingleton (MCont f) >< remTagsExp t

data TaggedIt s i a = Done a | Get (TaggedExp s (It i) i a)

addTags :: It i a -> Count s (TaggedIt s i a)
addTags (I.Done x) = pure $ Done x
addTags (I.Get me) = Get <$> addTagsExp me

{-
remTags :: TaggedIt s i a -> It i a
remTags (Done x) = I.Done x
remTags (
-}

(>>=!) :: TaggedIt s i a -> (a -> It i b) -> Count s (TaggedIt s i b)
Done x >>=! f = addTags $ f x
Get te >>=! f = do
	tg <- countup
	pure . Get $ te |> Tagged tg f

(>>=^) :: It i a -> TaggedExp s (It i) a b -> It i b
I.Done x >>=^ f = val f x
I.Get k >>=^ f = I.Get (k >< remTagsExp f)

get :: Count s (TaggedIt s i i)
get = do
	tg <- countup
	pure . Get . tsingleton $ Tagged tg pure

val :: TaggedExp s (It i) a b -> (a -> It i b)
val q = case tviewl q of
	TOne (Tagged _ f) -> f
	Tagged _ h :| t -> \x -> h x >>=^ t

{-
par' :: TaggedIt s i a -> TaggedIt s i a -> Count s (TaggedIt s i (TaggedIt s i a, TaggedIt s i a))
par' l r
	| Get f <- l, Get g <- r = do
		g <- get
		g >>=! \x -> par' (addTags $ val f x) (addTags $ val g x)
	| otherwise = pure $ Done (l, r)
	-}
