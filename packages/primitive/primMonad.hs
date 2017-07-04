{-# LANGUAGE MagicHash, TypeFamilies, UnboxedTuples #-}

import Control.Monad.Primitive
import System.Random.MWC

import GHC.Prim

data PureWorld

data Id a = Id { unId :: State# PureWorld -> (# State# PureWorld, a #) }

retI :: a -> Id a
retI x = Id $ \pw -> (# pw, x #)

bindI :: Id a -> (a -> Id b) -> Id b
bindI (Id x) f = Id $ \pw -> let (# pw', y #) = x pw in unId (f y) pw'

instance Functor Id where fmap = flip bindI . (retI .)

instance Applicative Id where
	pure = retI
	mf <*> mx = mf `bindI` \f -> mx `bindI` \x -> retI $ f x

instance Monad Id where return = retI; (>>=) = bindI

instance PrimMonad Id where
	type PrimState Id = PureWorld
	primitive = Id
