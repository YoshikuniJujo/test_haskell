{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.TaggableFunction (
	-- * TaggableFun
	TaggableFun ) where

import Control.Monad.Freer.Par.Funable (Funable(..), Taggable(..), Tag(..))

---------------------------------------------------------------------------

data TaggableFun m a b = Fun { tag :: Tag, unFun :: a -> m b }

instance Funable TaggableFun where fun = Fun NoTag; ($$) = unFun
instance Taggable TaggableFun where putTag f i = f { tag = Tag i }; getTag = tag
