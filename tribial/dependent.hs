{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}

import Data.Kind
import Data.Singletons

data Sigma a (b :: a -> *) where
	Sigma :: SingKind a => {
		proj1 :: Sing (x :: a),
		proj2 :: b x } -> Sigma a b

{-
data Migma a (b :: a -> *) where
	Migma :: {
		prj1 :: (x :: a),
		prj2 :: b x } -> Migma a b
		-}
