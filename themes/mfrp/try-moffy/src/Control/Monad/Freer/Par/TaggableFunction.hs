{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.TaggableFunction (TaggableFun) where

import Control.Monad.Freer.Par.Funable (Funable(..), Taggable(..), Tag(..))

---------------------------------------------------------------------------

data TaggableFun m a b where
	Fun :: Tag -> (a -> m b) -> TaggableFun m a b

instance Funable TaggableFun where fun = Fun NoTag; ($$) (Fun _ f) = f

instance Taggable TaggableFun where
	putTag (Fun _ f) i = Fun (Tag i) f
	getTag (Fun t _) = t
